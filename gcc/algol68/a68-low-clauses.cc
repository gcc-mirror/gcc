/* Lower clauses to GENERIC.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Given a serial_clause node P, return whether it performs dynamic stack
   allocations.

   This function allocates for the fact that the bottom-up parser generates
   successively nested serial clauses like

     SERIAL_CLAUSE
       SERIAL_CLAUSE
        ...

   the outer of which corresponds to a single serial clause in the source code,
   but it is the inner ones annotated by the dsa pass.  */

static bool
serial_clause_dsa (NODE_T *p)
{
  for (NODE_T *s = p; SUB (s) &&  IS (s, SERIAL_CLAUSE); s = SUB (s))
    {
      if (DYNAMIC_STACK_ALLOCS (s))
	return true;
    }

  return false;
}

/* Lower one or more labels.

     label : defining identifier, colon symbol;
             label, defining identifier, colon symbol;

   A label lowers into a LABEL_EXPR and the declaration of a LABEL_DECL in the
   current block and bind.  Lists of labels get returned in nested compound
   expressions.  */

tree
a68_lower_label (NODE_T *p, LOW_CTX_T ctx)
{
  tree expr = NULL_TREE;

  if (IS (SUB (p), LABEL))
    expr = a68_lower_tree (SUB (p), ctx);

  NODE_T *defining_identifier;

  if (IS (SUB (p), DEFINING_IDENTIFIER))
    defining_identifier = SUB (p);
  else
    {
      gcc_assert (IS (NEXT (SUB (p)), DEFINING_IDENTIFIER));
      defining_identifier = NEXT (SUB (p));
    }

  /* Create LABEL_DECL if necessary.  */
  tree label_decl = TAX_TREE_DECL (TAX (defining_identifier));
  if (label_decl == NULL_TREE)
    {
      label_decl = build_decl (a68_get_node_location (defining_identifier),
			       LABEL_DECL,
			       a68_get_mangled_identifier (NSYMBOL (defining_identifier)),
			       void_type_node);
      TAX_TREE_DECL (TAX (defining_identifier)) = label_decl;
    }

  a68_add_decl (label_decl);

  /* Return the accummulated LABEL_EXPRs.  */
  tree label_expr = build1 (LABEL_EXPR, void_type_node, label_decl);
  if (expr)
    return fold_build2_loc (a68_get_node_location (p),
			    COMPOUND_EXPR,
			    void_type_node,
			    expr, label_expr);
  else
    return label_expr;
}

/* Lower a labeled unit.

     labeled unit : label, unit.

   Lower the label, then the unit.  Return them in a compound expression.  */

tree
a68_lower_labeled_unit (NODE_T *p, LOW_CTX_T ctx)
{
  tree label_expr = a68_lower_tree (SUB (p), ctx);
  tree unit_expr = a68_lower_tree (NEXT (SUB (p)), ctx);

  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  TREE_TYPE (unit_expr),
			  label_expr, unit_expr);
}

/* Lower a completer.

     exit_symbol

   This handler replaces the last expression in stmt_list with a statement
   assigning it to the clause result of the current serial clause, then jump to
   the exit label of the current serial clause. Note that a completer is a
   separator so stmt_list contains at least one expression at this point.  Note
   that a completer can only appear inside a serial clause.

   This function always returns NULL_TREE, so the traversing code shall always
   be careful to travese on these nodes explicitly and ignore the returned
   value.  */

tree
a68_lower_completer (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  a68_add_completer ();
  return NULL_TREE;
}

/* Lower an initialiser series.

   Parse tree:

   initialiser series : serial clause, semi symbol, declaration list;
                        initialiser series, declaration list;
			initialiser series, semi symbol, unit;
			initialiser series, semi symbol, labeled unit;
			initialiser series, semi symbol, declaration list.

   GENERIC:

   Traverse subtree adding units and labels to STMT_LIST, and declarations to
   BLOCK.

   This function always returns NULL_TREE, so the traversing code shall always
   be careful to travese on these nodes explicitly and ignore the returned
   value.  */

tree
a68_lower_initialiser_series (NODE_T *p, LOW_CTX_T ctx)
{
  for (NODE_T *s = SUB (p); s != NO_NODE; FORWARD (s))
    {
      if (!IS (s, SEMI_SYMBOL))
	a68_add_stmt (a68_lower_tree (s, ctx));
    }
  return NULL_TREE;
}

/* Lower a serial clause.

     serial clause : labeled unit;
                     unit;
		     serial clause, semi symbol, unit;
		     serial clause, exit symbol, labeled unit;
		     serial clause, semi_symbol, declaration list;
		     initialiser series, semi symbol, unit;
		     initialiser series, semi symbol, labeled unit.

   Ranges:

     serial-clause
     ------------- R1

   See the function body to see the lowering actions.

   This function always returns NULL_TREE, so the traversing code shall always
   be careful to travese on these nodes explicitly and ignore the returned
   value.  */

tree
a68_lower_serial_clause (NODE_T *p, LOW_CTX_T ctx)
{
  if (IS (SUB (p), SERIAL_CLAUSE))
    {
      /* Traverse down for side-effects.  */
      (void) a68_lower_tree (SUB (p), ctx);

      if (IS (NEXT (SUB (p)), EXIT_SYMBOL))
	{
	  /* Traverse the completer for side-effects.  This turns the last
	     expression in the current statements list into an assignment.  */
	  (void) a68_lower_tree (NEXT (SUB (p)), ctx);
	  /* Now append the result of the labeled unit to the current
	     statements list.  */
	  a68_add_stmt (a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
	}
      else
	{
	  /* Append the result of either the unit or the declarations list in
	     the current statements list.  */
	  a68_add_stmt (a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
	}
    }
  else if (IS (SUB (p), INITIALISER_SERIES))
    {
      /* Traverse down for side-effects.  */
      (void) a68_lower_tree (SUB (p), ctx);

      /* Append the result of either the unit or the declarations list in the
	 current statements list.  */
      a68_add_stmt (a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
    }
  else
    {
      /* Append the result of either the unit or labeled unit in the current
	 statements list.  */
      a68_add_stmt (a68_lower_tree (SUB (p), ctx));
    }

  return NULL_TREE;
}

/* Lower a loop clause.

     loop clause : for part, from part, by part, to part, while part, alt do part;
                   for part, from part, by part, while part, alt do part;
		   for part, from part, while part, alt do part;
		   for part, by part, to part, while part, alt do part;
		   for part, by part, to part, while part, alt do part;
		   for part, by part, while part, alt do part;
		   for part, while part, alt do part;
		   for part, from part, by part, to part, alt do part;
		   for part, from part, by part, alt do part;
		   for part, from part, alt do part;
		   for part, by part, to part, alt do part;
		   for part, by part, alt do part;
		   for part, to part, alt do part,
		   for part, alt do part;
		   from part, by part, to part, while part, alt do part;
		   from part, by part, while aprt, alt do part;
		   from part, to part, while aprt, alt do part;
		   from part, while part, alt do part;
		   from part, by part, to part, alt do part;
		   from part, by part, alt do part;
		   from part, to part, alt do part;
		   from part, alt do part;
		   by part, to part, while part, alt do part;
		   by part, while part, alt do part;
		   by part, to part, alt do part;
		   by part, alt do part;
		   to part, while part, alt do part;
		   to part, alt do part;
		   while part, alt do part;
		   do part.
*/

tree
a68_lower_loop_clause (NODE_T *p ATTRIBUTE_UNUSED,
		       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  NODE_T *s = SUB (p);
  bool while_part = false;
  bool has_iterator = false;
  tree iterator = NULL_TREE;
  tree while_condition = NULL_TREE;
  tree do_part = NULL_TREE;
  tree from_part = NULL_TREE;
  tree by_part = NULL_TREE;
  tree to_part = NULL_TREE;
  tree overflow = NULL_TREE;
  NODE_T *iterator_defining_identifier = NO_NODE;

  if (IS (s, FOR_PART))
    {
      /* Get the defining identifier.  */
      iterator_defining_identifier = NEXT (SUB (s));
      has_iterator = true;
      FORWARD (s);
    }

  if (IS (s, FROM_PART))
    {
      /* Lower the unit.  */
      from_part = a68_lower_tree (NEXT (SUB (s)), ctx);
      has_iterator = true;
      FORWARD (s);
    }

  if (IS (s, BY_PART))
    {
      /* Lower the unit.  */
      by_part = a68_lower_tree (NEXT (SUB (s)), ctx);
      has_iterator = true;
      FORWARD (s);
    }

  if (IS (s, TO_PART))
    {
      /* Lower the unit.  */
      to_part = a68_lower_tree (NEXT (SUB (s)), ctx);
      has_iterator = true;
      FORWARD (s);
    }

  if (has_iterator)
    {
      /* Introduce a range that spans until the end of the loop clause.   */
      a68_push_range (M_VOID);

      /* Compute some defaults for not specified loop parts.  Note that to_part
	 defaults to max_int or min_int depending on the signedness of
	 by_part.  */
      if (from_part == NULL_TREE)
	from_part = integer_one_node;
      if (by_part == NULL_TREE)
	by_part = integer_one_node;
      if (to_part == NULL_TREE)
	{
	  to_part = fold_build3 (COND_EXPR,
				 a68_bool_type,
				 fold_build2 (LT_EXPR, a68_int_type, by_part,
					      build_int_cst (a68_int_type, 0)),
				 a68_int_minval (a68_int_type),
				 a68_int_maxval (a68_int_type));
	}

      /* If the user has specified an explicit iterator in the form of a
	 defining-identifier in a for-part, use it as the name in the iterator
	 declaration and install the resulting declaration in the taxes table
	 in order for applied identifiers in the rest of the loop to find it.
	 Otherwise, the iterator is not directly accessible by the
	 programmer.  */
      const char *iterator_name = (iterator_defining_identifier == NO_NODE
				   ? "iterator%"
				   : NSYMBOL (iterator_defining_identifier));
      iterator = a68_lower_tmpvar (iterator_name, a68_int_type, from_part);
      if (iterator_defining_identifier != NO_NODE)
	TAX_TREE_DECL (TAX (iterator_defining_identifier)) = iterator;

      /* The from_part and to_part expressions shall be evaluated once and once
	 only.  The expression for from_part is evaluated only once in the
	 initialization expression for iterator% above, but we need to put
	 to_part in a temporary since it is used in the loop body.  */
      to_part = a68_lower_tmpvar ("to_part%", TREE_TYPE (to_part), to_part);

      /* We need to detect overflow/underflow of the iterator.  */
      overflow = a68_lower_tmpvar ("overflow%", boolean_type_node,
				   boolean_false_node);
    }

  if (IS (s, WHILE_PART))
    {
      while_part = true;
      /* Introduce a range that spans until the end of the loop clause.  */
      a68_push_range (M_VOID);
      /* Process the enquiry clause, which yields a BOOL.  */
      a68_push_stmt_list (M_BOOL);
      (void) a68_lower_tree (NEXT (SUB (s)), ctx);
      while_condition = a68_pop_stmt_list ();
      FORWARD (s);
    }

  /* DO part.  */
  gcc_assert (IS (s, ALT_DO_PART) || IS (s, DO_PART));

  /* Build the loop's body.  */
  a68_push_range (NULL);
  {
    /* First lower the loop exit condition.  */
    if (has_iterator || while_part)
      {
	tree exit_condition = NULL_TREE;
	/* IF overflow OREL (by_part < 0 THEN iterator < to_part ELSE iterator > to_part) FI */
	if (has_iterator)
	  exit_condition = fold_build2 (TRUTH_ORIF_EXPR, boolean_type_node,
					overflow,
					fold_build3 (COND_EXPR,
						     a68_bool_type,
						     fold_build2 (LT_EXPR, a68_int_type, by_part,
								  build_int_cst (a68_int_type, 0)),
						     fold_build2 (LT_EXPR, a68_int_type,
								  iterator, to_part),
						     fold_build2 (GT_EXPR, a68_int_type,
								  iterator, to_part)));
	/* NOT while_condition */
	if (while_part)
	  {
	    tree while_exit_condition = fold_build1 (TRUTH_NOT_EXPR,
						     a68_bool_type,
						     while_condition);
	    if (has_iterator)
	      exit_condition = fold_build2 (TRUTH_ORIF_EXPR, a68_bool_type,
					    exit_condition, while_exit_condition);
	    else
	      exit_condition = while_exit_condition;
	  }

	if (exit_condition != NULL_TREE)
	  a68_add_stmt (fold_build1 (EXIT_EXPR, void_type_node, exit_condition));
      }

    /* Serial clauses in DO .. OD do not yield any value.  */
    bool dsa = serial_clause_dsa (NEXT (SUB (s)));
    bool local = NON_LOCAL (NEXT (SUB (s))) == NO_TABLE;
    a68_push_serial_clause_range (M_VOID, dsa && local);
    (void) a68_lower_tree (NEXT (SUB (s)), ctx);
    do_part = a68_pop_serial_clause_range ();
    a68_add_stmt (do_part);

    if (has_iterator)
      {
	/* Increment the iterator by BY_PART.  Detect overflow.
	   Given a + b = sum, overflows = ((~((a) ^ (b)) & ((a) ^ (sum))) < 0)
	   See OVERFLOW_SUM_SIGN in double-int.cc for an explanation
	   of this formula.
	*/
	tree type = TREE_TYPE (iterator);
	tree a = iterator;
	tree b = save_expr (by_part);
	tree sum = fold_build2 (PLUS_EXPR, type, a, b);
	a68_add_stmt (fold_build2 (MODIFY_EXPR, boolean_type_node,
				   overflow,
				   fold_build2 (LT_EXPR, boolean_type_node,
						fold_build2 (BIT_AND_EXPR, type,
							     fold_build1 (BIT_NOT_EXPR, type,
									  fold_build2 (BIT_XOR_EXPR, type,
										       a, b)),
							     fold_build2 (BIT_XOR_EXPR, type,
									  a, sum)),
						build_int_cst (a68_int_type, 0))));
	a68_add_stmt (fold_build2 (MODIFY_EXPR, type, iterator, sum));
      }
  }
  tree loop_body = a68_pop_range ();

  /* Finally build the LOOP_EXPR and exit the introduced ranges.  */
  tree loop_clause = fold_build1_loc (a68_get_node_location (p),
				      LOOP_EXPR, a68_void_type, loop_body);
  if (while_part)
    {
      a68_add_stmt (loop_clause);
      loop_clause = a68_pop_range ();
    }
  if (has_iterator)
    {
      a68_add_stmt (loop_clause);
      loop_clause = a68_pop_range ();
    }

  return loop_clause;
}

/* Lower a conformity clause.

     conformity clause : case part, conformity in part, out part, esac symbol;
                         case part, conformity in part, esac symbol;
			 case part, conformity in part, conformity ouse part;
                         open part, conformity choice, choice, close symbol;
                         open part, conformity choice, close symbol;
			 open part, conformity choice, brief conformity ouse part.

     conformity choice : then bar symbol, specified unit list;
                         then bar symbol, specified unit.

     specified unit list : specified unit list, comma symbol, specified unit;
                           specified unit list, specified unit.

     specified unit : specifier, colon symbol, unit.

     specifier : open symbol, declarer, identifier, close symbol;
                 open symbol, declarer, close symbol;
		 open symbol, void symbol, close symbol.
*/

static void
lower_unite_case_unit (NODE_T *p,
		       tree enquiry, MOID_T *enquiry_mode,
		       tree result, tree exit_label, LOW_CTX_T ctx)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, SPECIFIER))
	{
	  MOID_T *spec_moid = MOID (NEXT (SUB (p)));
	  NODE_T *spec_identifier = NEXT (NEXT (SUB (p)));
	  NODE_T *spec_unit = NEXT (NEXT (p));
	  const char *specifier_identifier_name = NULL;
	  if (IS (spec_identifier, IDENTIFIER))
	    specifier_identifier_name = NSYMBOL (spec_identifier);

	  tree overhead = a68_union_overhead (enquiry);
	  tree spec_value = NULL_TREE;
	  tree entry_selected = NULL_TREE;
	  if (IS_UNION (spec_moid))
	    {
	      /* The spec_moid is an united mode, which must be unitable to the
		 enquiry_mode.  */
	      gcc_assert (a68_is_unitable (spec_moid, enquiry_mode,
					   SAFE_DEFLEXING));

	      /* Build the entry_selected expression.

		 For each mode in spec_moid, determine the corresponding index
		 in enquiry_mode and add a check for it to the expression.  */
	      for (PACK_T *pack = PACK (spec_moid); pack != NO_PACK; FORWARD (pack))
		{
		  int index = a68_united_mode_index (enquiry_mode, MOID (pack));
		  tree expr = fold_build2 (EQ_EXPR,
					   boolean_type_node,
					   overhead,
					   build_int_cst (TREE_TYPE (overhead), index));
		  if (entry_selected == NULL_TREE)
		    entry_selected = expr;
		  else
		    entry_selected = fold_build2 (TRUTH_OR_EXPR,
						  boolean_type_node,
						  entry_selected,
						  expr);
		}

	      /* The spec_value is an union of mode spec_moid, with the
		 overhead translated from enquiry_mode.  */
	      tree spec_overhead
		= a68_union_translate_overhead (enquiry_mode, overhead, spec_moid);
	      a68_push_range (spec_moid);
	      spec_value = a68_lower_tmpvar ("spec_value%",
					     CTYPE (spec_moid),
					     a68_get_skip_tree (spec_moid));
	      a68_add_stmt (a68_union_set_overhead (spec_value, spec_overhead));
	      tree from_cunion = a68_union_cunion (enquiry);
	      tree to_cunion = a68_union_cunion (spec_value);
	      a68_add_stmt (a68_lower_memcpy (fold_build1 (ADDR_EXPR,
							   build_pointer_type (TREE_TYPE (to_cunion)),
							   to_cunion),
					      fold_build1 (ADDR_EXPR,
							   build_pointer_type (TREE_TYPE (from_cunion)),
							   from_cunion),
					      size_in_bytes (TREE_TYPE (to_cunion))));
	      a68_add_stmt (spec_value);
	      spec_value = a68_pop_range ();
	    }
	  else
	    {
	      int index = a68_united_mode_index (enquiry_mode, spec_moid);
	      spec_value = a68_union_alternative (enquiry, index);
	      entry_selected = fold_build2 (EQ_EXPR,
					    TREE_TYPE (overhead),
					    overhead,
					    build_int_cst (TREE_TYPE (overhead), index));
	    }

	  a68_push_range (M_VOID);
	  {
	    /* If the enquiry value is ascribed to an identifier in the case
	       entry then create a suitable declaration and turn the identifier
	       into a defining identifier.  */
	    if (specifier_identifier_name)
	      {
		tree united_value = a68_lower_tmpvar (specifier_identifier_name,
						      CTYPE (spec_moid), spec_value);
		TAX_TREE_DECL (TAX (spec_identifier)) = united_value;
	      }

	    /* Set result% to the lowering of the unit and jump to the end of
	       the enquiry clause.  */
	    a68_add_stmt (fold_build2 (MODIFY_EXPR, TREE_TYPE (result),
				       result, a68_lower_tree (spec_unit, ctx)));
	    a68_add_stmt (fold_build1 (GOTO_EXPR,
				       void_type_node,
				       exit_label));
	    a68_add_stmt (a68_get_skip_tree (M_VOID));
	  }
	  tree process_entry = a68_pop_range ();

	  /* IF index = overhead THEN process entry FI */
	  a68_add_stmt (fold_build3 (COND_EXPR,
				     a68_void_type,
				     entry_selected,
				     process_entry,
				     a68_get_skip_tree (M_VOID)));

	  FORWARD (p); /* Skip specifier.  */
	  FORWARD (p); /* Skip unit.  */
	  /* The unit is skipped in the for loop post-action.  */
	}
      else
	lower_unite_case_unit (SUB (p),
			       enquiry, enquiry_mode,
			       result, exit_label, ctx);
    }
}

tree
a68_lower_conformity_clause (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *conformity_clause_mode = MOID (p);

  /* CASE or OUSE.  */
  NODE_T *s = SUB (p);
  NODE_T *enquiry_node = NEXT (SUB (s));
  MOID_T *enquiry_mode = MOID (SUB (s));

  /* Push a binding environment for the enquiry clause.  */
  a68_push_range (conformity_clause_mode);

  /* Process the enquiry clause and put the resulting value in enquiry%.  */
  a68_push_stmt_list (enquiry_mode);
  (void) a68_lower_tree (enquiry_node, ctx);
  tree enquiry = a68_lower_tmpvar ("enquiry%",
				   CTYPE (enquiry_mode),
				   a68_pop_stmt_list ());

  /* Create a decl for result%.  */
  tree result = a68_lower_tmpvar ("result%",
				  CTYPE (conformity_clause_mode),
				  a68_get_skip_tree (conformity_clause_mode));

  /* Create an exit label.  */
  tree exit_label = build_decl (UNKNOWN_LOCATION,
				LABEL_DECL,
				get_identifier ("exit_label%"),
				void_type_node);
  DECL_CONTEXT (exit_label) = a68_range_context ();
  a68_add_decl (exit_label);
  a68_add_decl_expr (fold_build1 (DECL_EXPR, TREE_TYPE (exit_label), exit_label));

  /* IN.  */
  FORWARD (s);
  lower_unite_case_unit (NEXT (SUB (s)),
			 enquiry, enquiry_mode,
			 result, exit_label, ctx);

  /* OUT.  */
  FORWARD (s);
  switch (ATTRIBUTE (s))
    {
    case CHOICE:
    case OUT_PART:
      {
	bool dsa = serial_clause_dsa (NEXT (SUB (s)));
	bool local = NON_LOCAL (NEXT (SUB (s))) == NO_TABLE;
	a68_push_serial_clause_range (conformity_clause_mode, dsa && local);

	(void) a68_lower_tree (NEXT (SUB (s)), ctx);
	a68_add_stmt (fold_build2 (MODIFY_EXPR, TREE_TYPE (result),
				   result, a68_pop_serial_clause_range ()));
	a68_add_stmt (fold_build1 (GOTO_EXPR, void_type_node, exit_label));
	break;
      }
    case CLOSE_SYMBOL:
    case ESAC_SYMBOL:
      a68_add_stmt (fold_build2 (MODIFY_EXPR,
				 TREE_TYPE (result),
				 result,
				 a68_get_skip_tree (conformity_clause_mode)));
      a68_add_stmt (fold_build1 (GOTO_EXPR, void_type_node, exit_label));
      break;
    default:
      /* Recurse.

	 Note that the parser guarantees that the embedded CASE clause is a
	 conformity clause, and that its mode is the same than the containing
	 clause, but it doesn't annotate the mode in the tree node so we have
	 to do it here.  */
      MOID (s) = conformity_clause_mode;
      a68_add_stmt (fold_build2 (MODIFY_EXPR,
				 TREE_TYPE (result),
				 result,
				 a68_lower_conformity_clause (s, ctx)));
      a68_add_stmt (fold_build1 (GOTO_EXPR, void_type_node, exit_label));
      break;
    }

  /* ESAC */
  a68_add_stmt (build1 (LABEL_EXPR, void_type_node, exit_label));
  a68_add_stmt (result);
  return a68_pop_range ();
}

/* Lower a case clause.

     case clause : open part, case choice clause, choice, close symbol;
                   open part, case choice clause, close symbol;
		   open part, case shoice clause, brief ouse part;
		   case part, case in part, out part, esac symbol;
		   case part, case in part, esac symbol;
		   case part, case in part, case ouse part;
*/

static void
lower_int_case_unit (NODE_T *p,
		     tree enquiry, MOID_T *enquiry_mode,
		     tree result, tree exit_label, int *count,
		     LOW_CTX_T ctx)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, UNIT))
	{
	  a68_push_range (M_VOID);
	  {
	    /* Set result% to the lowering of the unit and jump to the end of
	       the enquiry clause.  */
	    a68_add_stmt (fold_build2 (MODIFY_EXPR, TREE_TYPE (result),
				       result, a68_lower_tree (p, ctx)));
	    a68_add_stmt (fold_build1 (GOTO_EXPR,
				       void_type_node,
				       exit_label));
	    a68_add_stmt (a68_get_skip_tree (M_VOID));
	  }
	  tree process_entry = a68_pop_range ();

	  /* IF count = enquiry THEN process entry FI */
	  a68_add_stmt (fold_build3 (COND_EXPR,
				     a68_void_type,
				     fold_build2 (EQ_EXPR,
						  TREE_TYPE (enquiry),
						  enquiry,
						  build_int_cst (TREE_TYPE (enquiry), *count)),
				     process_entry,
				     a68_get_skip_tree (M_VOID)));
	  *count += 1;
	}
      else
	lower_int_case_unit (SUB (p),
			     enquiry, enquiry_mode,
			     result, exit_label, count, ctx);
    }
}

tree
a68_lower_case_clause (NODE_T *p ATTRIBUTE_UNUSED,
		       LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  MOID_T *case_clause_mode = MOID (p);

  /* CASE or OUSE  */
  NODE_T *s = SUB (p);
  NODE_T *enquiry_node = NEXT (SUB (s));
  MOID_T *enquiry_mode = M_INT;

  /* Push a bingding environment fo the case clause.  */
  a68_push_range (case_clause_mode);

  /* Process the enquiry clause and put the result value in enquiry%.  */
  a68_push_stmt_list (enquiry_mode);
  (void) a68_lower_tree (enquiry_node, ctx);
  tree enquiry = a68_lower_tmpvar ("enquiry%",
				   CTYPE (enquiry_mode),
				   a68_pop_stmt_list ());
  /* Create a decl for result%.  */
  tree result = a68_lower_tmpvar ("result%",
				  CTYPE (case_clause_mode),
				  a68_get_skip_tree (case_clause_mode));

  /* Create an exit label.  */
  tree exit_label = build_decl (UNKNOWN_LOCATION,
				LABEL_DECL,
				get_identifier ("exit_label%"),
				void_type_node);
  DECL_CONTEXT (exit_label) = a68_range_context ();
  a68_add_decl (exit_label);
  a68_add_decl_expr (fold_build1 (DECL_EXPR, TREE_TYPE (exit_label), exit_label));

  /* IN.  */
  FORWARD (s);
  int count = 1;
  lower_int_case_unit (NEXT (SUB (s)),
		       enquiry, enquiry_mode,
		       result, exit_label, &count, ctx);

  /* OUT.  */
  FORWARD (s);
  switch (ATTRIBUTE (s))
    {
    case CHOICE:
    case OUT_PART:
      {
	bool dsa = serial_clause_dsa (NEXT (SUB (s)));
	bool local = NON_LOCAL (NEXT (SUB (s))) == NO_TABLE;
	a68_push_serial_clause_range (case_clause_mode, dsa && local);

	(void) a68_lower_tree (NEXT (SUB (s)), ctx);
	a68_add_stmt (fold_build2 (MODIFY_EXPR, TREE_TYPE (result),
				   result, a68_pop_serial_clause_range ()));
	a68_add_stmt (fold_build1 (GOTO_EXPR, void_type_node, exit_label));
	break;
      }
    case CLOSE_SYMBOL:
    case ESAC_SYMBOL:
      a68_add_stmt (fold_build2 (MODIFY_EXPR,
				 TREE_TYPE (result),
				 result,
				 a68_get_skip_tree (case_clause_mode)));
      a68_add_stmt (fold_build1 (GOTO_EXPR, void_type_node, exit_label));
      break;
    default:
      /* Recurse.

	 Note that the parser guarantees that the embedded CASE clause has the
	 same mode than the containing clause, but it doesn't annotate the OUSE
	 node with its mode so we have to do it here.  */
      MOID (s) = case_clause_mode;
      a68_add_stmt (fold_build2 (MODIFY_EXPR,
				 TREE_TYPE (result),
				 result,
				 a68_lower_case_clause (s, ctx)));
      a68_add_stmt (fold_build1 (GOTO_EXPR, void_type_node, exit_label));
      break;
    }

  /* ESAC */
  a68_add_stmt (build1 (LABEL_EXPR, void_type_node, exit_label));
  a68_add_stmt (result);
  return a68_pop_range ();
}

/* Lower an enquiry clause.

     enquiry clause : unit;
                      enquiry clause, semi symbol, unit;
                      enquiry clause, comma symbol, unit;
		      initialiser series, semi symbol, unit.

   The units and declarations in the enquiry clause get lowered into
   expressions and declaration nodes which are added to the current serial
   clause.

   This function always returns NULL_TREE, so the traversing code shall always
   be careful to travese on these nodes explicitly and ignore the returned
   value.  */

tree
a68_lower_enquiry_clause (NODE_T *p, LOW_CTX_T ctx)
{
  if (IS (SUB (p), UNIT))
    {
      a68_add_stmt (a68_lower_tree (SUB (p), ctx));
    }
  else if (IS (SUB (p), ENQUIRY_CLAUSE))
    {
      (void) a68_lower_tree (SUB (p), ctx);
      gcc_assert (IS (NEXT (NEXT (SUB (p))), UNIT));
      a68_add_stmt (a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
    }
  else
    {
      gcc_assert (IS (SUB (p), INITIALISER_SERIES));
      gcc_assert (IS (NEXT (NEXT (SUB (p))), UNIT));
      (void) a68_lower_tree (SUB (p), ctx);
      a68_add_stmt (a68_lower_tree (NEXT (NEXT (SUB (p))), ctx));
    }

  return NULL_TREE;
}

/* Lower a conditional clause.

     conditional clause : open part, choice, choice, close symbol;
                          open part, choice, close symbol;
			  open part, choice, brief elif part;
			  if part, then part, else part, fi symbol;
			  if part, then part, elif part;
			  if part, then part, fi symbol.

     if part : if symbol, enquiry clause;
               if symbol, initialiser series.

     then part : then symbol, serial clause;
                 then symbol, initialiser series.

     elif part : elif if part, then part, else part, fi symbol;
                 elif if part, then part, fi symbol;
		 elif if part, then part, elif part.

     else part : else symbol, serial clause;
                 else symbol, initialiser series.

     elif if part : elif symbol, enquiry clause.

     open part : open symbol, enquiry clause.

     choice : then bar symbol, serial clause;
              then bar symbol initialiser series.

     brief elif part : else open part, choice, choice, close symbol;
                       else open part, choice, close symbol;
		       else open part, choice, bief elif part.

     else open part : else bar symbol, enquiry clause;
                      else bar symbol, initialiser series.

   Ranges:

     IF enquiry-clause THEN expr ELSE expr FI
                            --- R2    ---- R3
     ---------------------------------------- R1

   The conditional clause lowers into:

     BIND_EXPR
       BIND_EXPR_VARS -> delcls in enquiry clause.
       BIND_EXPR_BODY
         STMT_LIST
           enquiry% = ...;
           COND_EXPR (enquiry%, then_expr, else_expr)  */

tree
a68_lower_conditional_clause (NODE_T *p, LOW_CTX_T ctx)
{
  tree then_expr = NULL_TREE;
  tree else_expr = NULL_TREE;

  MOID_T *conditional_clause_mode = MOID (p);
  MOID_T *effective_rows_mode = NO_MOID;
  bool is_rows = false;

  /* Push a binding environment for the conditional.  */
  a68_push_range (is_rows ? effective_rows_mode : conditional_clause_mode);

  /* Create a decl for %enquiry and add it to the bind's declaration chain.  */
  tree enquiry_decl = build_decl (UNKNOWN_LOCATION,
				  VAR_DECL,
				  NULL, /* Set below.  */
				  a68_bool_type);
  char *enquiry_name = xasprintf ("enquiry%d%%", DECL_UID(enquiry_decl));
  DECL_NAME (enquiry_decl) = get_identifier (enquiry_name);
  free (enquiry_name);
  DECL_INITIAL (enquiry_decl) = a68_get_skip_tree (M_BOOL);
  a68_add_decl (enquiry_decl);

  /* Add a DECL_EXPR for enquiry_decl%  */
  a68_add_stmt (fold_build1 (DECL_EXPR, a68_bool_type, enquiry_decl));

  /* IF or ELIF part.  */
  NODE_T *s = SUB (p);

  /* Process the enquiry clause.  */
  (void) a68_lower_tree (NEXT (SUB (s)), ctx);

  /* Assignation enquiry% = .. expr ..
     Note that since no completers are allowed in enquiry clauses,
     the last statement in the statement list has to be the unit
     yielding the boolean value.  */
  tree_stmt_iterator si = tsi_last (a68_range_stmt_list ());
  gcc_assert (TREE_TYPE (tsi_stmt (si)) != void_type_node);
  a68_add_stmt (fold_build2 (MODIFY_EXPR, a68_bool_type, enquiry_decl, tsi_stmt (si)));
  tsi_delink (&si);

  /* THEN part.  */
  FORWARD (s);
  bool dsa = serial_clause_dsa (NEXT (SUB (s)));
  bool local = NON_LOCAL (NEXT (SUB (s))) == NO_TABLE;
  a68_push_serial_clause_range (is_rows ? effective_rows_mode : conditional_clause_mode,
				dsa && local);
  (void) a68_lower_tree (NEXT (SUB (s)), ctx);
  then_expr = a68_pop_serial_clause_range ();

  /* ELSE part  */
  FORWARD (s);
  switch (ATTRIBUTE (s))
    {
    case CHOICE:
    case ELSE_PART:
      {
	bool dsa = serial_clause_dsa (NEXT (SUB (s)));
	bool local = NON_LOCAL (NEXT (SUB (s))) == NO_TABLE;
	a68_push_serial_clause_range (is_rows ? effective_rows_mode : conditional_clause_mode,
				      dsa && local);
	(void) a68_lower_tree (NEXT (SUB (s)), ctx);
	else_expr = a68_pop_serial_clause_range ();
	break;
      }
    case CLOSE_SYMBOL:
    case FI_SYMBOL:
      {
	else_expr = a68_get_skip_tree (is_rows ? effective_rows_mode : conditional_clause_mode);
	break;
      }
    default:
      {
	/* ELIF part.  Recurse.  */
	MOID (s) = conditional_clause_mode;
	else_expr = a68_lower_conditional_clause (s, ctx);
      }
    }

  /* Build the conditional clause's COND_EXPR.  */
  a68_add_stmt (fold_build3_loc (a68_get_node_location (p),
				 COND_EXPR,
				 CTYPE (is_rows ? effective_rows_mode : conditional_clause_mode),
				 enquiry_decl,
				 then_expr, else_expr));

  return a68_pop_range ();
}

/* Lower a comma separated list of zero, two, or more units

     unit list : unit list, comma symbol, unit;
                 unit list, unit.

   The list of units lowers into appending the units into the current
   statements list.

   This function always returns NULL_TREE, so the traversing code shall always
   be careful to traverse on these nodes explicitly and ignore the returned
   value.  */

tree
a68_lower_unit_list (NODE_T *p, LOW_CTX_T ctx)
{
  if (IS (SUB (p), UNIT_LIST))
    (void) a68_lower_tree (SUB (p), ctx);

  for (NODE_T *s = SUB (p); s != NO_NODE; FORWARD (s))
    {
      if (IS (s, UNIT))
	a68_add_stmt (a68_lower_tree (s, ctx));
    }

  return NULL_TREE;
}

/* Lower a collateral clause.

     collateral clause : open symbol, unit list, close symbol;
                         open symbol, close symbol;
                         begin symbol, unit list, end symbol;
			 begin symbol, end symbol.

   An empty collateral clause lowers into EMPTY.  */

tree
a68_lower_collateral_clause (NODE_T *p ATTRIBUTE_UNUSED,
			     LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  bool clause_is_empty = (ATTRIBUTE (NEXT (SUB (p))) != UNIT_LIST);
  MOID_T *mode = MOID (p);

  /* Lower the constituent units into a statements list.  */
  a68_push_stmt_list (mode);
  if (!clause_is_empty)
    (void) a68_lower_tree (NEXT (SUB (p)), ctx);
  tree units = a68_pop_stmt_list ();

  /* The collateral clause lowers to different constructions depending on its
     nature.  */
  if (mode == M_VOID)
    {
      /* A VOID-collateral-clause lowers into a STMT_LIST containing all
	 the units.  Since there cannot be declarations in a collateral
	 clause, there is no need to introduce a new binding scope.  Note
	 that for now we are not really elaborating collaterally, but
	 sequentially.  */
      return units;
    }
  else if (IS_FLEXETY_ROW (mode) || mode == M_STRING)
    {
      if (mode == M_STRING)
	mode = M_FLEX_ROW_CHAR;

      /* This is a row display.  It lowers to a multiple.  */
      tree row_type = CTYPE (mode);
      size_t dim = DIM (DEFLEX (mode));

      if (clause_is_empty)
	{
	  /* The clause is empty.  This lowers into a multiple with DIM
	     dimension, each dimension having bounds of 1:0, and no
	     elements.  */
	  tree element_pointer_type = a68_row_elements_pointer_type (row_type);
	  tree multiple_elements = build_int_cst (element_pointer_type, 0);
	  tree multiple_elements_size = size_zero_node;

	  tree *lower_bounds = (tree *) xmalloc (sizeof (tree) * dim);
	  tree *upper_bounds = (tree *) xmalloc (sizeof (tree) * dim);
	  tree ssize_one_node = fold_convert (ssizetype, size_one_node);
	  tree ssize_zero_node = fold_convert (ssizetype, size_zero_node);
	  for (size_t d = 0; d < dim; ++d)
	    {
	      lower_bounds[d] = ssize_one_node;
	      upper_bounds[d] = ssize_zero_node;
	    }

	  tree row = a68_row_value (row_type, dim,
				    multiple_elements,
				    multiple_elements_size,
				    lower_bounds, upper_bounds);
	  TREE_CONSTANT (row) = 1;
	  free (lower_bounds);
	  free (upper_bounds);
	  return row;
	}

      if (dim == 1)
	{
	  /* Create a constructor with the multiple's elements.  */
	  vec <constructor_elt, va_gc> *ve = NULL;
	  int num_units = 0;
	  for (tree_stmt_iterator si = tsi_start (units); !tsi_end_p (si); tsi_next (&si))
	    {
	      tree unit = tsi_stmt (si);
	      if (A68_TYPE_HAS_ROWS_P (TREE_TYPE (unit)))
		unit = a68_low_dup (unit);
	      CONSTRUCTOR_APPEND_ELT (ve, size_int (num_units), unit);
	      num_units += 1;
	    }

	  tree element_pointer_type = a68_row_elements_pointer_type (row_type);
	  tree array_constructor_type = build_array_type (TREE_TYPE (element_pointer_type),
							  build_index_type (size_int (num_units - 1)));
	  tree array_constructor = build_constructor (array_constructor_type, ve);
	  tree multiple_elements = fold_build1 (ADDR_EXPR,
						element_pointer_type,
						array_constructor);
	  tree elements_type = TREE_TYPE (element_pointer_type);
	  tree multiple_elements_size = fold_build2 (MULT_EXPR, sizetype,
						     size_int (num_units),
						     size_in_bytes (elements_type));
	  tree lower_bound = fold_convert (ssizetype, size_one_node);
	  tree upper_bound = ssize_int (num_units);
	  tree row = a68_row_value (row_type, dim,
				    multiple_elements,
				    multiple_elements_size,
				    &lower_bound, &upper_bound);
	  return row;
	}
      else
	{
	  gcc_assert (dim > 1);

	  /* The units in the collateral clause are multiples, whose elements
	     are to be copied consecutively in a new multiple.  The descriptor
	     of this multiple is constructed as follows:

	     The first dimension is:

	     - The lower bound is 1.
	     - The upper bound is the number of sub-multiples processed
	       here.
             - The stride is the number of elements in each sub-multiple
	       multiplied by the element size.

	     Subsequent dimensions are taken from the first inner multiple.
	     All descriptors of the inner multiples shall be equal.  This is
	     checked at run-time, and in case of discrepancy a run-time error
	     is emitted.

             Let's see an example.  Suppose in the stmt-list we have:

               (1, 2, 3)
                 {triplets: {lb: 1 ub: 3 stride: 1S} elements: {1, 2, 3}}
               (4, 5, 6)
                 {triplets: {lb: 1 ub: 3 stride: 1S} elements: {4, 5, 6}}

             The resulting multiple would be:

               ((1, 2, 3), (4, 5, 6))
                 {triplets: {{lb: 1 ub: 2 stride: 3S}, {lb: 1 ub: 3 stride: 1S}}
                  elements: {1, 2, 3, 4, 5, 6}}  */

	  tree *lower_bounds = (tree *) xmalloc (sizeof (tree) * dim);
	  tree *upper_bounds = (tree *) xmalloc (sizeof (tree) * dim);
	  size_t num_units = 0;

	  for (tree_stmt_iterator si = tsi_start (units); !tsi_end_p (si); tsi_next (&si))
	    num_units++;

	  a68_push_range (mode);

	  /* Process each sub-multiple.  The first sub-multiple establishes the
	     bounds that all subsequent sub-multiples shall match.  */
	  tree multiple_elements = NULL_TREE;
	  tree multiple_elements_size = NULL_TREE;
	  tree sub_multiple = NULL_TREE;
	  //	  tree sub_multiple_lb = NULL_TREE;
	  //	  tree sub_multiple_ub = NULL_TREE;
	  //	  tree sub_multiple_stride = NULL_TREE;
	  tree index = a68_lower_tmpvar ("index%", sizetype, size_zero_node);
	  for (tree_stmt_iterator si = tsi_start (units); !tsi_end_p (si); tsi_next (&si))
	    {
	      if (sub_multiple == NULL)
		sub_multiple = a68_lower_tmpvar ("sub_multiple%",
						 TREE_TYPE (tsi_stmt (si)),
						 tsi_stmt (si));
	      else
		a68_add_stmt (fold_build2 (MODIFY_EXPR,
					   TREE_TYPE (tsi_stmt (si)),
					   sub_multiple,
					   tsi_stmt (si)));

	      if (si == tsi_start (units))
		{
#if 0
		  tree ssize_zero_node = fold_convert (ssizetype, size_zero_node);
		  /* The first sub-multiple establishes the bounds that all
		     subsequent sub-multiples shall match.  */
		  sub_multiple_lb = a68_lower_tmpvar ("sub_multiple_lb%",
						      ssizetype,
						      a68_multiple_lower_bound (sub_multiple,
										ssize_zero_node));
		  sub_multiple_ub = a68_lower_tmpvar ("sub_multiple_ub%",
						      ssizetype,
						      a68_multiple_upper_bound (sub_multiple,
										ssize_zero_node));
		  sub_multiple_stride = a68_lower_tmpvar ("sub_multiple_stride%",
							  sizetype,
							  a68_multiple_stride (sub_multiple,
									       size_zero_node));
#endif
		  /* Now we have enough information to calculate the size of
		     the elements of the new multiple and allocate
		     multiple_elements.  */
		  tree sub_multiple_elements = a68_multiple_elements (sub_multiple);
		  tree elements_pointer_type = TREE_TYPE (sub_multiple_elements);
		  tree elements_type = TREE_TYPE (elements_pointer_type);
		  multiple_elements_size = fold_build2 (MULT_EXPR, sizetype,
							     size_int (num_units),
							     size_in_bytes (elements_type));
		  multiple_elements_size = fold_build2 (MULT_EXPR, sizetype,
							multiple_elements_size,
							a68_multiple_num_elems (sub_multiple));
		  multiple_elements = a68_lower_tmpvar ("multiple_elements%",
							elements_pointer_type,
							a68_lower_alloca (elements_type,
									  multiple_elements_size));

		  /* We can also now calculate the bounds of the new multiple.
		     The top-level triplet has lower bound 1, upper bound is
		     num_units, and stride is the number of elements in each
		     sub-multiple multiplied by the element size.  Bounds for
		     the subsequent DIM-1 dimensions are copied from the
		     sub-multiple's descriptor.  */
		  lower_bounds[0] = fold_convert (ssizetype, size_one_node);
		  upper_bounds[0] = ssize_int (num_units);
		  for (size_t d = 1; d < dim; ++d)
		    {
		      lower_bounds[d] = a68_multiple_lower_bound (sub_multiple,
								  ssize_int (d - 1));
		      upper_bounds[d] = a68_multiple_upper_bound (sub_multiple,
								  ssize_int (d - 1));
		    }
		}
	      else
		{
		  /* Check bounds of this sub-multiple.  Note that this is
		     always done at run-time, since the interpretation of a row
		     display depens on the target type, whether it is a row row
		     or a row of rows, for example.  */
		  // XXX use sub_multiple_lb, sub_multiple_ub and sub_multiple_stride
		}

	      /* Copy the elements of a copy of the sub-multiple in the
		 elements of the multiple.  */
	      tree sub_multiple_elements = a68_multiple_elements (sub_multiple);
	      // XXX should we make a copy of the sub_multiple_elements here?
	      // We DO need to iterate slicing, because of strides: if
	      // the sub_multiple is a trimmer.
	      tree sub_multiple_elements_type = TREE_TYPE (sub_multiple_elements);
	      tree sub_multiple_num_elems = a68_multiple_num_elems (sub_multiple);
	      tree sub_multiple_element_type = TREE_TYPE (sub_multiple_elements_type);
	      tree sub_multiple_elements_size = fold_build2 (MULT_EXPR, sizetype,
							     sub_multiple_num_elems,
							     size_in_bytes (sub_multiple_element_type));

	      /* memcpy (multiple_elements[index], sub_multiple_elements)  */
	      a68_add_stmt (a68_lower_memcpy (fold_build2 (POINTER_PLUS_EXPR,
							   sub_multiple_elements_type,
							   multiple_elements,
							   index),
					      sub_multiple_elements,
					      sub_multiple_elements_size));
	      /* index += sub_multiple_elements_size */
	      a68_add_stmt (fold_build2 (MODIFY_EXPR, sizetype,
					 index,
					 fold_build2 (PLUS_EXPR, sizetype,
						      index, sub_multiple_elements_size)));
	    }

	  tree multiple = a68_lower_tmpvar ("multiple%",
					    row_type,
					    a68_row_value (row_type, dim,
							   multiple_elements,
							   multiple_elements_size,
							   lower_bounds, upper_bounds));
	  free (lower_bounds);
	  free (upper_bounds);

	  /* Yield the multiple.  */
	  a68_add_stmt (multiple);
	  return a68_pop_range ();
	}
    }
  else if (IS_STRUCT (mode))
    {
      /* This is a struct display.  There are as many units in the clause as
	 fields in the struct type.  Build a constructor with the values for
	 the fields.  */
      vec <constructor_elt, va_gc> *ve = NULL;
      tree_stmt_iterator si = tsi_start (units);
      for (tree f = TYPE_FIELDS (CTYPE (mode)); f; f = DECL_CHAIN (f))
	{
	  tree v = tsi_stmt (si);
	  gcc_assert (v != NULL_TREE);
	  v = a68_consolidate_ref (a68_type_moid (TREE_TYPE (f)) ,v);
	  CONSTRUCTOR_APPEND_ELT (ve, f, v);
	  tsi_next (&si);
	}
      tree ctor = build_constructor (CTYPE (mode), ve);
      return ctor;
    }
  else
    gcc_unreachable ();
}

/* Lower a parallel clause.

     parallel clause : par symbol, collateral clause.
*/

tree
a68_lower_parallel_clause (NODE_T *p ATTRIBUTE_UNUSED,
			   LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* XXX For now treat like a VOID collateral clause.  */
  return a68_lower_tree (NEXT (SUB (p)), ctx);
}

/* Lower a closed clause.

     closed clause : open symbol, serial clause, close symbol;
                     open symbol, initialiser series, close symbol;
		     begin symbol, serial clause, end symbol;
		     begin symbol, initialiser series, end symbol;

  This function returns a BIND_EXPR.  */

tree
a68_lower_closed_clause (NODE_T *p, LOW_CTX_T ctx)
{
  /* Determine the mode of the closed clause.  */
  MOID_T *clause_mode = MOID (p);
  gcc_assert (clause_mode != NO_MOID);
  gcc_assert (CTYPE (clause_mode) != NULL_TREE);

  /* Lower the enclosed serial clause.

     Note that a serial clause can be nested right inside another, and in that
     case the range we are pushing corresponds to all of them, so we have to
     keep this into account when determining whether using a DSA serial
     range.  */

  bool dsa = serial_clause_dsa (NEXT (SUB (p)));
  bool local = NON_LOCAL (NEXT (SUB (p))) == NO_TABLE;
  a68_push_serial_clause_range (clause_mode, dsa && local);
  (void) a68_lower_tree (NEXT (SUB (p)), ctx);
  return a68_pop_serial_clause_range ();
}

/* Lower an access clause.

     access clause : access symbol, joined module indication sequence,
                       enclosed clause.
*/

tree
a68_lower_access_clause (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *controlled_clause = NEXT (NEXT_SUB (p));

  a68_push_range (MOID (p));

  /* Call preludes of all ACCESSed modules.  */
  for (NODE_T *q = SUB (p); q != NO_NODE; FORWARD (q))
    {
      if (IS (q, MODULE_INDICANT))
	{
	  TAG_T *tag = a68_find_tag_global (TABLE (q), MODULE_SYMBOL, NSYMBOL (q));
	  gcc_assert (tag != NO_TAG);
	  MOIF_T *moif = MOIF (tag);
	  gcc_assert (moif != NO_MOIF);
	  const char *prelude = PRELUDE (moif);

	  tree prelude_decl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
					  get_identifier (prelude),
					  build_function_type_list (void_type_node,
								    void_type_node,
								    NULL_TREE));
	  DECL_EXTERNAL (prelude_decl) = 1;
	  TREE_PUBLIC (prelude_decl) = 1;
	  a68_add_decl (prelude_decl);
	  a68_add_stmt (build_call_expr_loc (a68_get_node_location (q),
					     prelude_decl, 0));
	}
    }

  /* Now the controlled clause.  */
  tree controlled_clause_tree = a68_lower_tree (controlled_clause, ctx);
  tree tmp = a68_lower_tmpvar ("accessed_clause_result%",
			       TREE_TYPE (controlled_clause_tree),
			       controlled_clause_tree);

  /* Call postludes of all ACCESSed modules.  */
  for (NODE_T *q = SUB (p); q != NO_NODE; FORWARD (q))
    {
      if (IS (q, MODULE_INDICANT))
	{
	  TAG_T *tag = a68_find_tag_global (TABLE (q), MODULE_SYMBOL, NSYMBOL (q));
	  gcc_assert (tag != NO_TAG);
	  MOIF_T *moif = MOIF (tag);
	  gcc_assert (moif != NO_MOIF);
	  const char *postlude = POSTLUDE (moif);

	  tree postlude_decl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
					   get_identifier (postlude),
					   build_function_type_list (void_type_node,
								     void_type_node,
								     NULL_TREE));
	  DECL_EXTERNAL (postlude_decl) = 1;
	  TREE_PUBLIC (postlude_decl) = 1;
	  a68_add_decl (postlude_decl);
	  a68_add_stmt (build_call_expr_loc (a68_get_node_location (q),
					     postlude_decl, 0));
	}
    }

  a68_add_stmt (tmp);
  return a68_pop_range ();
}

tree a68_lower_access_clauses (NODE_T *p, LOW_CTX_T ctx);

/* Lower an enclosed clause.

     enclosed clause : parallel clause; closed clause;
                       collateral clause; conditional clause;
		       case clause; conformity clause;
		       loop clause ; access clause.
*/

tree
a68_lower_enclosed_clause (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_lower_tree (SUB (p), ctx);
}
