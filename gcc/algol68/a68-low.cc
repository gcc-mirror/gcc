/* Lower the Algol 68 parse tree to GENERIC.
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

/* Return a tree with an identifier for the mangled version of a given
   name.

   Publicized symbols feature the module name publishing them.
   Internal symbols don't.

   Bold words, i.e. mode indicants, module indicants and operator indicants,
   are mangled to upper-case.

   Tags, i.e. identifiers are mangled to lower-case.

   Monads and nomads are mangled to letter codes symbolizing the symbols:

     %    (p)ercentage
     ^    (c)aret
     &    (a)mpersand
     +    pl(u)s
     -    (m)inus
     ~    (t)ilde
     !    (b)ang
     ?    (q)uestion mark
     >    bi(g)ger than
     <    (l)ess than
     /    (s)lash
     =    (e)qual
     :    c(o)lon
     *    sta(r)

   Each letter code is followed by a single underscore character.  */

static tree
get_mangled_identifier_or_indicant (const char *name, bool indicant,
				    const char *mname, bool internal,
				    bool numbered)
{
  /* First determine the size of the mangled symbol.  */
  size_t mangled_size = strlen (name) + 1;
  if (mname)
    {
      /* Add size for MNAME_ */
      mangled_size += strlen (mname) + 1;
      if (internal)
	/* Another _ */
	mangled_size += 1;
    }
  for (const char *p = name; *p; ++p)
    {
      mangled_size += 1;
      if (strchr (MONADS, *p) != NULL
	  || strchr (NOMADS, *p) != NULL
	  || *p == ':')
	/* Each monad or nomad requires two chars to encode.  */
	mangled_size += 1;
    }

  char *number_buf = NULL;
  if (numbered)
    {
      static unsigned int cnt;
      number_buf = xasprintf ("%d", cnt++);
      mangled_size += strlen (number_buf);
    }

  /* Now fill-in the mangled symbol.  */
  char *mangled_name = (char *) alloca (mangled_size);
  size_t pos = 0;
  if (mname)
    {
      for (const char *p = mname; *p; ++p)
	/* Module names are bold words.  Make sure to emit them in upper-case.  */
	mangled_name[pos++] = TOUPPER (*p);
      mangled_name[pos++] = '_';
      if (internal)
	mangled_name[pos++] = '_';
    }

  for (const char *p = name; *p; ++p)
    {
      if (strchr (MONADS, *p) != NULL
	  || strchr (NOMADS, *p) != NULL
	  || *p == ':')
	{
	  char c;
	  switch (*p)
	    {
	    case '%': c = 'p'; break;
	    case '^': c = 'c'; break;
	    case '&': c = 'a'; break;
	    case '+': c = 'u'; break;
	    case '-': c = 'm'; break;
	    case '~': c = 't'; break;
	    case '!': c = 'b'; break;
	    case '?': c = 'q'; break;
	    case '>': c = 'g'; break;
	    case '<': c = 'l'; break;
	    case '/': c = 's'; break;
	    case '=': c = 'e'; break;
	    case ':': c = 'o'; break;
	    case '*': c = 'r'; break;
	    default:
	      /* Should not happen.  */
	      gcc_unreachable ();
	    }
	  mangled_name[pos++] = c;
	  mangled_name[pos++] = '_';
	}
      else
	{
	  if (indicant)
	    mangled_name[pos++] = TOUPPER (*p);
	  else
	    mangled_name[pos++] = TOLOWER (*p);
	}
    }

  if (numbered)
    {
      for (char *p = number_buf; *p; ++p)
	mangled_name[pos++] = *p;
      free (number_buf);
    }
  mangled_name[pos++] = '\0';

  return get_identifier (mangled_name);
}

tree
a68_get_mangled_identifier (const char *name, const char *mname,
			    bool internal, bool numbered)
{
  return get_mangled_identifier_or_indicant (name, false /* indicant */, mname,
					     internal, numbered);
}

tree
a68_get_mangled_indicant (const char *name, const char *mname,
			  bool internal, bool numbered)
{
  return get_mangled_identifier_or_indicant (name, true /* indicant */, mname,
					     internal, numbered);
}

/* Demangle a given SYMBOL.

   This function does the reverse operation than
   get_mangled_identifier_or_indicant.  */

char *
a68_demangle_symbol (const char *mname, const char *symbol,
		     bool is_operator)
{
  gcc_assert (strlen (symbol) >= strlen (mname) + 1);
  /* First get rid of the module name and underscore.  */
  symbol += strlen (mname) + 1;
  /* Now demangle the rest.  */
  size_t size = strlen (symbol) + 1;
  char *demangled = (char *) xmalloc (size + 1);
  size_t o = 0;
  for (size_t i = 0; i < size; ++i)
    {
      if (symbol[i+1] == '_')
	{
	  switch (symbol[i])
	    {
	    case 'p': demangled[o++] = '+'; break;
	    case 'c': demangled[o++] = '^'; break;
	    case 'a': demangled[o++] = '&'; break;
	    case 'u': demangled[o++] = '+'; break;
	    case 'm': demangled[o++] = '-'; break;
	    case 't': demangled[o++] = '~'; break;
	    case 'b': demangled[o++] = '!'; break;
	    case 'q': demangled[o++] = '?'; break;
	    case 'g': demangled[o++] = '>'; break;
	    case 'l': demangled[o++] = '<'; break;
	    case 's': demangled[o++] = '/'; break;
	    case 'e': demangled[o++] = '='; break;
	    case 'o': demangled[o++] = ':'; break;
	    case 'r': demangled[o++] = '*'; break;
	    default:
	      /* Invalid mangling.  */
	      // XXX this should be checked at import time in extract.
	      gcc_unreachable ();
	    }
	  i += 1;
	}
      else
	demangled[o++] = symbol[i];
    }
  demangled[o] = '\0';

  if (is_operator)
    {
      /* Remove trailing digits.  */
      for (size_t i = strlen (demangled) - 1; i > 0; --i)
	{
	  if (ISDIGIT (demangled[i]))
	    demangled[i] = '\0';
	  else
	    break;
	}
    }

  return demangled;
}

/* Return a tree with the EMPTY value.

   EMPTY is the only denotation of the VOID mode.  It is used in unions to
   denote "no value".  It must have size zero, so it lowers into an empty
   constructor with zero elements of type void.  This is what GNU C uses to
   implement the empty struct extension.  */

tree
a68_get_empty (void)
{
  return build_constructor (a68_void_type, NULL);
}

/* Return a tree with the yielding of SKIP of a given mode.

   SKIP stands for some value of some given mode.  It shall be used only in a
   context where the compiler can determine the mode.

   The particular value to which it elaborates is non-important, but this
   compiler always uses the same values.  See the a68_get_ref_*_tree functions
   for details on what values are these.  */

tree
a68_get_skip_tree (MOID_T *m)
{
  tree expr = NULL_TREE;

  while (EQUIVALENT (m) != NO_MOID)
    m = EQUIVALENT (m);

  if (IS_INTEGRAL (m))
    expr = a68_get_int_skip_tree (m);
  else if (m == M_CHAR)
    expr = a68_get_char_skip_tree ();
  else if (m == M_BOOL)
    expr = a68_get_bool_skip_tree ();
  else if (IS_REAL (m))
    expr = a68_get_real_skip_tree (m);
  else if (IS_BITS (m))
    expr = a68_get_bits_skip_tree (m);
  else if (IS_REF (m))
    expr = a68_get_ref_skip_tree (m);
  else if (IS (m, PROC_SYMBOL))
    expr = a68_get_proc_skip_tree (m);
  else if (IS_STRUCT (m))
    expr = a68_get_struct_skip_tree (m);
  else if (IS_UNION (m))
    expr = a68_get_union_skip_tree (m);
  else if (IS_FLEXETY_ROW (m))
    expr = a68_get_multiple_skip_tree (m);
  else if (m == M_STRING)
      expr = a68_get_string_skip_tree ();
  else if (m == M_ROWS  || IS (m, SERIES_MODE))
    {
      /* XXX assert that all modes in the series are rows? */
      tree rows_type = CTYPE (M_ROWS);
      tree dim_field = TYPE_FIELDS (rows_type);
      tree triplets_field = TREE_CHAIN (dim_field);
      tree null_pointer = build_int_cst (TREE_TYPE (triplets_field), 0);
      expr = build_constructor_va (rows_type, 2,
				   dim_field, size_zero_node,
				   triplets_field, null_pointer);
    }
  else if (m == M_VOID || m == M_HIP)
    expr = a68_get_empty ();
  else
    {
      fatal_error (UNKNOWN_LOCATION,
		   "get skip tree: cannot compute SKIP for mode %s",
		   a68_moid_to_string (m, MOID_ERROR_WIDTH, NODE (m), true));
      gcc_unreachable ();
    }

  return expr;
}

/* Given a tree node EXP holding a value of mode M:

   *NUM_REFS is set to the number of REFs in M.

   *NUM_POINTERS is set to the number of pointers in the type of EXP that
   correspond to the REFs in M.  */

void
a68_ref_counts (tree exp, MOID_T *m, int *num_refs, int *num_pointers)
{
  /* Count REFs in M and pointers in the type of EXP.  Note that VAR_DECLs
     corresponding to REF PROC are of type pointer, so these should not count
     for the count!  */

  /* Make sure we are accessing the real mode definition.  */
  while (EQUIVALENT (m) != NO_MOID)
    m = EQUIVALENT (m);

  *num_refs = 0;
  *num_pointers = 0;
  for (MOID_T *s = m; s != NO_MOID && IS_REF (s); s = SUB (s))
    *num_refs += 1;
  for (tree p = TREE_TYPE (exp);
       p != NULL_TREE && POINTER_TYPE_P (p) && TREE_CODE (TREE_TYPE (p)) != FUNCTION_TYPE;
       p = TREE_TYPE (p))
    *num_pointers += 1;

  gcc_assert (*num_refs >= *num_pointers);
}

/* The Algol 68 variable declaration

     [LOC|HEAP] AMODE foo;

   Is in principle equivalent to the identity declaration

     REF AMODE foo = [LOC|HEAP] AMODE;

   In both cases the object ascribed to the defining identifier `foo' is of
   mode REF AMODE.  The ascribed object is a name which is created by a
   generator implied in the actual declarer in the first case, and an explicit
   generator in the initialization expression in the second case.

   However, this front-end implements these two cases differently in order to
   reduce the amount of both indirect addressing and of storage:

   - The variable declaration `[LOC|HEAP] AMODE foo;' lowers into a VAR_DECL
     with type ATYPE provided that the generator is LOC and that it contains no
     rows.  Accessing it requires direct addressing.  When its address is
     required, an ADDR_EXPR shall be used.

   - The identity declaration `REF AMODE foo = LOC AMODE;' lowers into a
     VAR_DECL with type *ATYPE.  Accessing it requires indirect addressing.  It
     is effectively a pointer.

   This introduces the complication that an expression (the VAR_DECL) whose
   type is TYPE can appear in a place where *TYPE is expected.  This function,
   given the required mode and an expression, adds as many ADDR_EXPR to EXPR as
   necessary so the resulting value is of the required type.  Other than this
   nuisance, the parser guarantees that the entities have the right type at the
   location they appear, so a call to a68_consolidate_ref is all must be needed
   at any point in the lowering process to guarantee a valid value for the
   context.

   This function expects:
   - That the type of EXPR is zero or more pointers to a base type BTYPE.
   - That the mode M is zero or more REFs to a base non-ref mode AMODE.
   - That the number of pointers in the type of EXPR is less or equal than the
     number of REFs in the mode M.
   - That BTYPE and AMODE are equivalent.  */

tree
a68_consolidate_ref (MOID_T *m, tree expr)
{
  int num_refs, num_pointers;
  a68_ref_counts (expr, m, &num_refs, &num_pointers);

  /* Address EXPR as many times as necessary to match the number of REFs in the
     desired mode.  */
  while (num_pointers < num_refs)
    {
      if (TREE_CODE (expr) == COMPOUND_EXPR)
	{
	  /* (..., x) -> (..., &x) */
	  //	  gcc_assert (TREE_CODE (TREE_OPERAND (expr, 0)) == MODIFY_EXPR);
	  //	  gcc_assert (VAR_P (TREE_OPERAND (expr, 1)));
	  TREE_OPERAND (expr, 1) = a68_consolidate_ref (m, TREE_OPERAND (expr, 1));
	  TREE_TYPE (expr) = TREE_TYPE (TREE_OPERAND (expr, 1));
	}
      else
	{
	  /* x -> &x */
	  if (TREE_CODE (expr) == INDIRECT_REF)
	    /* expr is an indirection.  Remove the pointer rather than adding
	       an addr.  This avoids &* situations and marking stuff as
	       addressable unnecessarily.  */
	    expr = TREE_OPERAND (expr,0);
	  else
	    {
	      TREE_ADDRESSABLE (expr) = true;
	      expr = fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (expr)), expr);
	    }
	}
      num_pointers += 1;
    }

  return expr;
}

/* Make a declaration for an anonymous routine of mode MODE.  */

tree
a68_make_anonymous_routine_decl (MOID_T *mode)
{
  /* The CTYPE of MODE is a pointer to a function.  We need the pointed
     function type for the FUNCTION_DECL.  */
  tree func_type = TREE_TYPE (CTYPE (mode));
  tree func_decl = build_decl (UNKNOWN_LOCATION,
			       FUNCTION_DECL,
			       NULL_TREE /* name, set below.  */,
			       func_type);
  char *name = xasprintf ("routine%d", DECL_UID (func_decl));
  DECL_NAME (func_decl) = a68_get_mangled_identifier (name);
  free (name);
  DECL_EXTERNAL (func_decl) = 0;
  DECL_STATIC_CHAIN (func_decl) = !a68_in_global_range ();
  /* Nested functions should be addressable.
     XXX this should be propagated to their containing functions, so for now
     we mark them all as addressable.  */
  TREE_ADDRESSABLE (func_decl) = 1;
  /* A nested function is not global.  */
  TREE_PUBLIC (func_decl) = a68_in_global_range ();
  TREE_STATIC (func_decl) = 1;

  return func_decl;
}

/* Make a declaration for a constant procedure or operator.  */

tree
a68_make_proc_identity_declaration_decl (NODE_T *identifier,
					 const char *module_name,
					 bool indicant, bool external,
					 const char *extern_symbol)
{
  /* The CTYPE of MODE is a pointer to a function.  We need the pointed
     function type for the FUNCTION_DECL.  */
  tree func_type = TREE_TYPE (CTYPE (MOID (identifier)));
  bool public_range = PUBLIC_RANGE (TABLE (TAX (identifier)));
  tree func_decl = build_decl (UNKNOWN_LOCATION,
			       FUNCTION_DECL,
			       NULL_TREE, /* name, set below.  */
			       func_type);

  if (public_range)
    {
      bool publicized = PUBLICIZED (TAX (identifier));

      DECL_EXTERNAL (func_decl) = 0;
      if (publicized)
	TREE_PUBLIC (func_decl) = 1;
      else
	TREE_PUBLIC (func_decl) = 0;
      if (indicant)
	DECL_NAME (func_decl)
	  = a68_get_mangled_indicant (NSYMBOL (identifier), module_name,
				      false /* internal */,
				      (IS (identifier, DEFINING_OPERATOR)
				       || IS (identifier, OPERATOR)));
      else
	DECL_NAME (func_decl)
	  = a68_get_mangled_identifier (NSYMBOL (identifier), module_name,
					false /* internal */,
					(IS (identifier, DEFINING_OPERATOR)
					 || IS (identifier, OPERATOR)));
    }
  else if (external)
    {
      DECL_EXTERNAL (func_decl) = 1;
      TREE_PUBLIC (func_decl) = 1;
      DECL_NAME (func_decl) = get_identifier (extern_symbol);
    }
  else
    {
      DECL_EXTERNAL (func_decl) = 0;
      DECL_STATIC_CHAIN (func_decl) = !a68_in_global_range ();
      /* Nested functions should be addressable.
	 XXX this should be propagated to their containing functions, so for now
	 we mark them all as addressable.  */
      TREE_ADDRESSABLE (func_decl) = 1;
      /* A nested function is not global.  */
      TREE_PUBLIC (func_decl) = a68_in_global_range ();
      if (indicant)
	DECL_NAME (func_decl) = a68_get_mangled_indicant (NSYMBOL (identifier));
      else
	DECL_NAME (func_decl) = a68_get_mangled_identifier (NSYMBOL (identifier));
    }
  TREE_STATIC (func_decl) = 1;

  return func_decl;
}

/* Make a declaration for an identity declaration.  */

tree
a68_make_identity_declaration_decl (NODE_T *identifier,
				    const char *module_name,
				    bool indicant, bool external,
				    const char *extern_symbol)
{
  tree type = CTYPE (MOID (identifier));
  bool public_range = PUBLIC_RANGE (TABLE (TAX (identifier)));
  tree decl = build_decl (a68_get_node_location (identifier),
			  VAR_DECL,
			  NULL_TREE, /* name, set below.  */
			  type);

  if (public_range)
    {
      bool publicized = PUBLICIZED (TAX (identifier));

      DECL_EXTERNAL (decl) = 0;
      TREE_STATIC (decl) = 1;
      if (publicized)
	TREE_PUBLIC (decl) = 1;
      else
	TREE_PUBLIC (decl) = 0;
      if (indicant)
	DECL_NAME (decl) = a68_get_mangled_indicant (NSYMBOL (identifier), module_name,
						     false /* internal */,
						     (IS (identifier, DEFINING_OPERATOR)
						      || IS (identifier, OPERATOR)));
      else
	DECL_NAME (decl) = a68_get_mangled_identifier (NSYMBOL (identifier), module_name,
						       false /* internal */,
						       (IS (identifier, DEFINING_OPERATOR)
							|| IS (identifier, OPERATOR)));
    }
  else if (external)
    {
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
      DECL_NAME (decl) = get_identifier (extern_symbol);
    }
  else
    {
      DECL_EXTERNAL (decl) = 0;
      TREE_PUBLIC (decl) = 0;
      DECL_NAME (decl) = a68_get_mangled_identifier (NSYMBOL (identifier));
    }

  DECL_INITIAL (decl) = a68_get_skip_tree (MOID (identifier));
  return decl;
}

/* Make a declaration for a variable declaration.
   The mode of the given identifier is expected to be a REF AMODE.  */

tree
a68_make_variable_declaration_decl (NODE_T *identifier,
				    const char *module_name,
				    bool external,
				    const char *extern_symbol)
{
  gcc_assert (IS_REF (MOID (identifier)));

  MOID_T *mode = MOID (identifier);
  bool use_pointer = (HEAP (TAX (identifier)) != STATIC_SYMBOL
		      && ((HEAP (TAX (identifier)) == HEAP_SYMBOL)
			  || HAS_ROWS (SUB (MOID (identifier)))));
  bool public_range = PUBLIC_RANGE (TABLE (TAX (identifier)));
  tree type = use_pointer ? CTYPE (mode) : CTYPE (SUB (mode));
  tree decl = build_decl (a68_get_node_location (identifier),
			  VAR_DECL,
			  NULL_TREE, /* name, set below.  */
			  type);

  if (public_range)
    {
      bool publicized = PUBLICIZED (TAX (identifier));

      DECL_EXTERNAL (decl) = 0;
      TREE_STATIC (decl) = 1;
      if (publicized)
	TREE_PUBLIC (decl) = 1;
      else
	TREE_PUBLIC (decl) = 0;
      DECL_NAME (decl) =  a68_get_mangled_identifier (NSYMBOL (identifier), module_name,
						      false /* internal */,
						      (IS (identifier, DEFINING_OPERATOR)
						       || IS (identifier, OPERATOR)));
    }
  else if (external)
    {
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
      DECL_NAME (decl) = get_identifier (extern_symbol);
    }
  else
    {
      TREE_PUBLIC (decl) = 0;
      DECL_NAME (decl) =  a68_get_mangled_identifier (NSYMBOL (identifier));
    }

  DECL_INITIAL (decl) = a68_get_skip_tree (use_pointer ? mode : SUB (mode));
  return decl;
}

/* Do a checked indirection.

   P is a tree node used for its location information.
   EXP is an expression that gets indirected.
   EXP_MODE is the mode of exp.  */

tree
a68_checked_indirect_ref (NODE_T *p, tree exp, MOID_T *exp_mode)
{
  tree exp_type = TREE_TYPE (exp);
  tree nil_check = NULL_TREE;

  if (OPTION_NIL_CHECKING (&A68_JOB))
    {
      exp = save_expr (exp);
      tree consolidated_exp = a68_consolidate_ref (exp_mode, exp);

      /* Check whether we are dereferencing NIL.  */
      unsigned int lineno = NUMBER (LINE (INFO (p)));
      const char *filename_str = FILENAME (LINE (INFO (p)));
      tree filename = build_string_literal (strlen (filename_str) + 1,
					    filename_str);
      tree call = a68_build_libcall (A68_LIBCALL_DEREFNIL,
					 void_type_node, 2,
					 filename,
					 build_int_cst (unsigned_type_node, lineno));
      call = fold_build2 (COMPOUND_EXPR, a68_bool_type, call, boolean_false_node);
      nil_check = fold_build2 (NE_EXPR, exp_type,
			       consolidated_exp,
			       build_int_cst (exp_type, 0));
      nil_check = fold_build2 (TRUTH_ORIF_EXPR, exp_type,
			       nil_check, call);
    }

  tree deref = fold_build1 (INDIRECT_REF, TREE_TYPE (exp_type), exp);
  if (nil_check == NULL_TREE)
    return deref;
  else
    return fold_build2 (COMPOUND_EXPR, TREE_TYPE (deref),
			nil_check, deref);
}

/* Deref a given expression EXP whose mode is MOID (P).

   The value to dereference always corresponds to a name, but it may consist
   of:

   - Not a pointer, in which case corresponds to a name lowered to a VAR_DECL.

   - A pointer to a function, in which case corresponds to a name of mode REF
     PROC, lowered to a VAR_DECL.

   - Any other pointer corresponds to a name lowered to a VAR_DECL that is a
     pointer.

   In the first two cases, in both r-value and l-value situations the expected
   result is achieved by just returning the value: in r-value the decl denotes
   the value, in l-value the decl denotes the (direct) address of the
   value.  */

tree
a68_low_deref (tree exp, NODE_T *p)
{
  int num_refs, num_pointers;
  a68_ref_counts (exp, MOID (p), &num_refs, &num_pointers);

  if (num_refs > num_pointers)
    return exp;
  else
    {
      gcc_assert (num_refs == num_pointers);
      return a68_checked_indirect_ref (p, exp, MOID (p));
    }
}

/* Get a deep-copy of a given Algol 68 value EXP.  */

tree
a68_low_dup (tree expr, bool use_heap)
{
  tree dup = NULL_TREE;
  tree type = TREE_TYPE (expr);

  /* XXX */
  use_heap = true;

  /* Determine the mode corresponding to the type of EXPR.  */
  MOID_T *m = a68_type_moid (type);
  gcc_assert (m != NO_MOID);
  while (EQUIVALENT (m) != NO_MOID)
    m = EQUIVALENT (m);

  if (A68_ROW_TYPE_P (type))
    {
      /* We need to copy the elements as well as the descriptor.  There is no
	 need to check bounds.  */

      /* Deflexe the mode as appropriate.  */
      while (IS_FLEX (m))
	m = SUB (m);
      gcc_assert (IS_ROW (m) || m == M_STRING);

      a68_push_range (NULL);

      /* First allocate space for the dupped elements.  */
      expr = save_expr (expr);
      tree elements = a68_multiple_elements (expr);
      tree element_pointer_type = TREE_TYPE (elements);
      tree element_type = TREE_TYPE (element_pointer_type);
      tree new_elements_size = save_expr (a68_multiple_elements_size (expr));
      tree new_elements_type = TREE_TYPE (TREE_TYPE (elements));
      MOID_T *new_elements_moid = a68_type_moid (new_elements_type);
      tree new_elements = a68_lower_tmpvar ("new_elements%",
					    TREE_TYPE (elements),
					    (use_heap
					     ? a68_lower_malloc (new_elements_moid,
								 new_elements_size)
					     : a68_lower_alloca (new_elements_moid,
								 new_elements_size)));

      /* Then copy the elements.

	 If the mode of the elements stored in the multiple dont have rows,
	 then we can just use memcpy.  Otherwise, we have to loop and recurse
	 to dup all the elements in the multiple one by one.

	 The above applies to multiples of any number of dimensions.  */
      if (m == M_STRING || !HAS_ROWS (SUB (m)))
	{
	  a68_add_stmt (a68_lower_memcpy (new_elements,
					  elements,
					  new_elements_size));
	  a68_add_stmt (new_elements);
	}
      else
	{
	  /* Note that num_elems includes elements that are not accessible due
	     to trimming.  */
	  tree num_elems = a68_lower_tmpvar ("numelems%", size_type_node,
					     fold_build2 (TRUNC_DIV_EXPR, sizetype,
							  new_elements_size,
							  size_in_bytes (element_type)));
	  tree orig_elements = a68_lower_tmpvar ("orig_elements%",
						 element_pointer_type, elements);
	  tree index = a68_lower_tmpvar ("index%", size_type_node, size_zero_node);

	  /* Begin of loop body.  */
	  a68_push_range (NULL);

	  /* if (index == num_elems) break; */
	  a68_add_stmt (fold_build1 (EXIT_EXPR,
				     void_type_node,
				     fold_build2 (EQ_EXPR,
						  size_type_node,
						  index, num_elems)));
	  /* new_elements[index] = elements[index] */
	  tree offset = fold_build2 (MULT_EXPR, sizetype,
				     index, size_in_bytes (element_type));
	  tree new_elem_lvalue = fold_build2 (MEM_REF, element_type,
					      fold_build2 (POINTER_PLUS_EXPR,
							   element_pointer_type,
							   new_elements,
							   offset),
					      fold_convert (element_pointer_type,
							    integer_zero_node));
	  tree elem = fold_build2 (MEM_REF, element_type,
				   fold_build2 (POINTER_PLUS_EXPR,
						element_pointer_type,
						orig_elements,
						offset),
				   fold_convert (element_pointer_type,
						 integer_zero_node));
	  a68_add_stmt (fold_build2 (MODIFY_EXPR, element_type,
				     new_elem_lvalue,
				     a68_low_dup (elem, use_heap)));
	  /* index++ */
	  a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR,
				     size_type_node,
				     index, size_one_node));
	  tree loop_body = a68_pop_range ();
	  /* End of loop body.  */

	  a68_add_stmt (fold_build1 (LOOP_EXPR,
				     void_type_node,
				     loop_body));
	  a68_add_stmt (new_elements);
	}

      new_elements = a68_pop_range ();
      TREE_TYPE (new_elements) = element_pointer_type;

      /* Now build a descriptor pointing to the dupped elements and return it.
	 Note that the descriptor is always allocated on the stack.  */
      dup = a68_row_value_raw (type,
			       a68_multiple_triplets (expr),
			       new_elements,
			       new_elements_size);
    }
  else if (!HAS_ROWS (m))
    {
      /* Non-multiple values that do not contain rows do not need to be dupped,
	 since they can be just moved around using the semantics of
	 MODIFY_EXPR.  */
      dup = expr;
    }
  else if (A68_STRUCT_TYPE_P (type))
    {
      /* Since struct value can contain multiples and unions and other values
	 that require deep copy, we cannot simply rely on the C semantics of a
	 MODIFY_EXPR.  */
      tree struct_type = type;
      vec <constructor_elt, va_gc> *ce = NULL;

      expr = save_expr (expr);
      for (tree field = TYPE_FIELDS (struct_type);
	   field;
	   field = TREE_CHAIN (field))
	{
	  CONSTRUCTOR_APPEND_ELT (ce, field,
				  a68_low_dup (fold_build3 (COMPONENT_REF,
							    TREE_TYPE (field),
							    expr,
							    field,
							    NULL_TREE),
					       use_heap));
	}
      dup = build_constructor (struct_type, ce);
    }
  else if (A68_UNION_TYPE_P (type))
    {
      /* We need to recurse in whatever type corresponding to the active mode
	 in the united value.  This shall be done at run-time by using a series
	 of

	   IF overhead IS index of mode blah in union
	   THEN dup = dup_type (CTYPE (mode blah in union))
	   FI
      */

      MOID_T *union_mode = a68_type_moid (type);

      a68_push_range (union_mode);
      dup = a68_lower_tmpvar ("dup%", type, expr);

      tree cunion_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type)));
      tree field_decl = TYPE_FIELDS (cunion_type);
      while (EQUIVALENT (union_mode) != NO_MOID)
	union_mode = EQUIVALENT (union_mode);
      for (PACK_T *pack = PACK (union_mode); pack != NO_PACK; FORWARD (pack))
	{
	  tree continue_label_decl = build_decl (UNKNOWN_LOCATION,
						 LABEL_DECL,
						 NULL, /* Set below.  */
						 void_type_node);
	  char *label_name = xasprintf ("continue%d%%", DECL_UID (continue_label_decl));
	  DECL_NAME (continue_label_decl) = get_identifier (label_name);
	  free (label_name);

	  a68_add_decl (continue_label_decl);

	  a68_add_stmt (fold_build2 (TRUTH_ORIF_EXPR,
				     integer_type_node,
				     fold_build2 (EQ_EXPR,
						  integer_type_node,
						  a68_union_overhead (dup),
						  size_int (a68_united_mode_index (union_mode, MOID (pack)))),
				     fold_build2 (COMPOUND_EXPR,
						  integer_type_node,
						  build1 (GOTO_EXPR, void_type_node, continue_label_decl),
						  integer_zero_node)));
	  a68_add_stmt (fold_build2 (MODIFY_EXPR, type,
				     fold_build3 (COMPONENT_REF,
						  TREE_TYPE (field_decl),
						  a68_union_cunion (dup),
						  field_decl,
						  NULL_TREE),
				     a68_low_dup (fold_build3 (COMPONENT_REF,
							       TREE_TYPE (field_decl),
							       a68_union_cunion (dup),
							       field_decl,
							       NULL_TREE),
						  use_heap)));
	  a68_add_stmt (build1 (LABEL_EXPR, void_type_node, continue_label_decl));
	  field_decl = TREE_CHAIN (field_decl);
	}

      a68_add_stmt (dup);
      dup = a68_pop_range ();
    }
  else
    /* Not an Algol 68 value.  */
    gcc_unreachable ();

  return dup;
}

/* Lower code to ascribe the value yielded by the expression in RHS to the
   defining identifier implied by the LHS, which is a VAR_DECL tree.  MODE is
   the mode of the value to be ascribed.  */

tree
a68_low_ascription (MOID_T *mode, tree lhs, tree rhs)
{
  gcc_assert (VAR_P (lhs));

  tree type = CTYPE (mode);
  if (IS (mode, PROC_SYMBOL))
    {
      /* A pointer to a function, or a function, is expected at the right hand
	 side.  We need a pointer for the left hand side..  */
      if (TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE)
	{
	  type = build_pointer_type (type);
	  rhs = fold_build1 (ADDR_EXPR, type, rhs);
	}
    }

  if (HAS_ROWS (mode))
    rhs = a68_low_dup (rhs);
  return fold_build2 (MODIFY_EXPR, type, lhs, rhs);
}

/* Perform an assignation of RHS to LHS.

   MODE_RHS is the mode of the rhs.
   MODE_LHS is the mode of the lhs.

   MODE_LHS shall be REF [FLEX] MODE_LHS.  */

tree
a68_low_assignation (NODE_T *p,
		     tree lhs, MOID_T *mode_lhs,
		     tree rhs, MOID_T *mode_rhs)
{
  NODE_T *lhs_node = SUB (p);
  tree assignation = NULL_TREE;
  tree orig_rhs = rhs;

  if (IS_FLEXETY_ROW (mode_rhs))
    {
      /* Make a deep copy of the rhs.  Note that we have to use the heap
	 because the scope of the lhs may be older than the scope of the rhs.
	 XXX this can be ommitted if a68_multiple_copy_elems below supports
	 overlapping multiples.  */
      if (HAS_ROWS (mode_rhs))
	rhs = a68_low_dup (rhs, true /* use_heap */);
      rhs = save_expr (rhs);

      /* Determine whether the REF [FLEX] MODE_LHS is flexible.  */
      if (SUB (mode_lhs) == M_STRING || IS_FLEX (SUB (mode_lhs)))
	{
	  /* Assigning to a flexible name updates descriptor with new bounds
	     and also sets the elements to the dup of the rhs.  No boundscheck
	     is peformed.  XXX but bound checking in contained values may be
	     necessary, ghost elements.  */
	  if (POINTER_TYPE_P (TREE_TYPE (lhs))
	      && TREE_TYPE (TREE_TYPE (lhs)) == TREE_TYPE (rhs))
	    {
	      /* Make sure to not evaluate the expression yielding the pointer
		 more than once.  */
	      lhs = save_expr (lhs);
	      tree deref_lhs = a68_checked_indirect_ref (lhs_node, lhs, mode_lhs);
	      assignation = fold_build2 (COMPOUND_EXPR,
					 TREE_TYPE (lhs),
					 fold_build2 (MODIFY_EXPR, TREE_TYPE (lhs),
						      deref_lhs, rhs),
					 lhs);
	    }
	  else
	    {
	      /* The lhs is either a variable or a component ref as a l-value.  It
		 is ok to evaluate it as an r-value as well as doing so inroduces
		 no side-effects.  */
	      assignation = fold_build2 (COMPOUND_EXPR,
					 TREE_TYPE (lhs),
					 fold_build2 (MODIFY_EXPR, TREE_TYPE (lhs),
						      lhs, rhs),
					 lhs);
	    }
	}
      else
	{
	  /* Dereference the multiple at the left-hand side.  This may require
	     indirection.  */

	  tree effective_lhs;
	  if (POINTER_TYPE_P (TREE_TYPE (lhs)))
	    {
	      /* The name at the lhs is a pointer.  */
	      gcc_assert (TREE_TYPE (TREE_TYPE (lhs)) == TREE_TYPE (rhs));
	      lhs = save_expr (lhs);
	      effective_lhs = a68_checked_indirect_ref (lhs_node, lhs, mode_lhs);
	    }
	  else
	    {
	      /* The name at the lhs is either a variable or a component ref as
		 a l-value.  It is ok to evaluate it as an r-value as well as
		 doing so introduces no side-effects.  */
	      effective_lhs = lhs;
	    }

	  /* Copy over the elements in a loop.  The space occupied by the
	     previous elements stored in the lhs multiple will be recovered by
	     either stack shrinkage or garbage collected.  */
	  tree copy_elements = a68_multiple_copy_elems (mode_rhs, effective_lhs, rhs);
	  assignation = fold_build2 (COMPOUND_EXPR,
				     TREE_TYPE (lhs),
				     copy_elements,
				     lhs);

	  /* Check the bounds of the multiple at the rhs to make sure they are
	     the same than the bounds of the multiple already referred by the
	     lhs.  If the bounds don't match then emit a run-time error.  */
	  if (OPTION_BOUNDS_CHECKING (&A68_JOB))
	    assignation = fold_build2 (COMPOUND_EXPR,
				       TREE_TYPE (assignation),
				       a68_multiple_bounds_check_equal (p,
									effective_lhs,
									rhs),
				       assignation);

	}
    }
  else
    {
      /* First make sure we got a pointer in the RHS in case it is a name.  */
      rhs = a68_consolidate_ref (mode_rhs, rhs);

      /* The assignation implies copying the entire value being assigned, so
	 make sure we do a deep copy whenever needed.  Note that we have to use
	 the heap because the scope of the lhs may be older than the scope of
	 the rhs.  */
      if (HAS_ROWS (mode_rhs))
	rhs = a68_low_dup (rhs, true /* use_heap */);

      if (POINTER_TYPE_P (TREE_TYPE (lhs))
	  && TREE_TYPE (TREE_TYPE (lhs)) == TREE_TYPE (rhs))
	{
	  /* If the left hand side is a pointer, deref it, but return the
	     pointer.  Make sure to not evaluate the expression yielding the
	     pointer more than once.  */
	  lhs = save_expr (lhs);
	  tree deref_lhs = a68_checked_indirect_ref (lhs_node, lhs, mode_lhs);
	  assignation = fold_build2 (COMPOUND_EXPR,
				     TREE_TYPE (lhs),
				     fold_build2 (MODIFY_EXPR, TREE_TYPE (lhs),
						  deref_lhs, rhs),
				     lhs);
	}
      else
	{
	  /* Otherwise the lhs is either a variable or a component ref as an
	     l-value.  It is ok to evaluate it as an r-value as well as doing
	     so introduces no side-effects.  */
	  assignation = fold_build2 (COMPOUND_EXPR,
				     TREE_TYPE (lhs),
				     fold_build2 (MODIFY_EXPR, TREE_TYPE (lhs),
						  lhs, rhs),
				     lhs);
	}
    }

  /* Since it is been assigned to a name, the rhs is no longer constant.  */
  if (A68_ROW_TYPE_P (TREE_TYPE (orig_rhs)) || A68_STRUCT_TYPE_P (TREE_TYPE (orig_rhs)))
    TREE_CONSTANT (orig_rhs) = 0;
  return assignation;
}

/* Build a tree that copies SIZE bytes from SRC into DST.  */

tree
a68_lower_memcpy (tree dst, tree src, tree size)
{
  return build_call_expr (builtin_decl_explicit (BUILT_IN_MEMCPY), 3,
			  dst, src, size);
}

/* Build a tree that allocates SIZE bytes on the stack and returns a *TYPE
   pointer to it.  */

tree
a68_lower_alloca (MOID_T *m, tree size)
{
  tree type = CTYPE (m);
  tree call = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
  call = build_call_expr_loc (UNKNOWN_LOCATION, call, 2,
			      size,
			      size_int (TYPE_ALIGN (type)));
  call = fold_convert (build_pointer_type (type), call);
  return call;
}


/* Build a tree that allocates SIZE bytes on the heap and returns a pointer to
   a tree with type equivalent to mode M.  */

tree
a68_lower_malloc (MOID_T *m, tree size)
{
  tree type = CTYPE (m);
  a68_libcall_fn libcall
    = (HAS_REFS (m) || HAS_ROWS (m)) ? A68_LIBCALL_MALLOC : A68_LIBCALL_MALLOC_LEAF;
  return fold_convert (build_pointer_type (type),
		       a68_build_libcall (libcall, ptr_type_node,
					  1, size));
}

/* Build code for a temporary variable named NAME, of type TYPE and initialized
   to INIT.  Returns the decl node for the temporary.  */

tree
a68_lower_tmpvar (const char *name, tree type, tree init)
{
  tree tmpvar = build_decl (UNKNOWN_LOCATION,
			    VAR_DECL,
			    get_identifier (name),
			    type);
  DECL_ARTIFICIAL (tmpvar) = 1;
  DECL_IGNORED_P (tmpvar) = 1;
  a68_add_decl (tmpvar);
  a68_add_decl_expr (fold_build1 (DECL_EXPR, type, tmpvar));
  a68_add_stmt (fold_build2 (INIT_EXPR, type, tmpvar, init));
  return tmpvar;
}

/* Build a FUNC_DECL for a top-level non-public function and return it.  */

tree
a68_low_toplevel_func_decl (const char *name, tree fntype)
{
  tree fndecl = build_decl (UNKNOWN_LOCATION,
			    FUNCTION_DECL,
			    NULL /* set below */,
			    fntype);
  char *_name = xasprintf ("__ga68_%s%d", name, DECL_UID (fndecl));
  DECL_NAME (fndecl) = get_identifier (_name);
  free (_name);
  DECL_EXTERNAL (fndecl) = 0;
  TREE_PUBLIC (fndecl) = 0;
  TREE_STATIC (fndecl) = 1;

  return fndecl;
}

/* Build a PARM_DECL whose context is TYPE with the given NAME.  */

tree
a68_low_func_param (tree fndecl, const char *name, tree type)
{
  tree param = build_decl (UNKNOWN_LOCATION, PARM_DECL,
			   get_identifier (name), type);
  DECL_CONTEXT (param) = fndecl;
  DECL_ARG_TYPE (param) = TREE_TYPE (param);
  layout_decl (param, 0);
  return param;
}

/* Lower revelations, to calls to either its prelude or poslude.
   This function always returns NULL_TREE.  */

static tree
lower_revelations (NODE_T *p, LOW_CTX_T ctx, bool prelude)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      lower_revelations (SUB (p), ctx, prelude);
      if (IS (p, MODULE_INDICANT))
	{
	  tree decl = build_decl (a68_get_node_location (p),
				  FUNCTION_DECL,
				  NULL_TREE, /* name, set below.  */
				  build_function_type (void_type_node,
						       void_list_node));
	  DECL_NAME (decl)
	    = a68_get_mangled_identifier (prelude ? "_prelude" : "_postlude",
					  NSYMBOL (p));
	  DECL_EXTERNAL (decl) = 1;
	  TREE_PUBLIC (decl) = 1;
	  a68_add_decl (decl);
	  a68_add_stmt (build_call_expr_loc (a68_get_node_location (p),
					     decl, 0));
	}
    }

  return NULL_TREE;
}

/* Lower a module text.

     module text : revelation part, def part, postlude part, fed symbol ;
                   revelation part, def part, fed symbol ;
		   def part, postlude part, fed symbol ;
		   def part, postlude part, fed symbol.
     def part : def symbol, enquiry clause.
     postlude part : postlude symbol, serial clause.

   Each module text lowers to two functions which are callable from outside the
   current compilation unit:

     MODULENAME__prelude
     MODULENAME__postlude

   Declarations inside prelude and postlude are lowered into global decl trees.
   The non-PUBlicized ones are marked as static.

   This handler always returns NULL_TREE.  */

static tree
lower_module_text (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *def_part = (IS (SUB (p), REVELATION_PART)
		      ? NEXT_SUB (p)
		      : SUB (p));
  NODE_T *revelation_part = (IS (SUB (p), REVELATION_PART)
			     ? SUB (p)
			     : NO_NODE);
  NODE_T *postlude_part = (IS (NEXT (def_part), FED_SYMBOL)
			   ? NO_NODE
			   : NEXT (def_part));
  NODE_T *prelude_enquiry = NEXT_SUB (def_part);

  /* The global sentinel of the module, initialized to 0.  */
  tree sentinel_decl = build_decl (UNKNOWN_LOCATION,
				   VAR_DECL, NULL /* name */,
				   sizetype);
  char *sentinel_name = xasprintf ("%s__sentinel", ctx.module_definition_name);
  DECL_NAME (sentinel_decl) = get_identifier (sentinel_name);
  free (sentinel_name);
  TREE_PUBLIC (sentinel_decl) = 0;
  TREE_STATIC (sentinel_decl) = 1;
  DECL_CONTEXT (sentinel_decl) = NULL_TREE;  /* File scope.  */
  make_decl_rtl (sentinel_decl);
  varpool_node::finalize_decl (sentinel_decl);

  /* Create the prelude function.  */
  tree prelude_decl = build_decl (a68_get_node_location (def_part),
				  FUNCTION_DECL,
				  NULL_TREE, /* name, set below.  */
				  build_function_type (void_type_node,
						       void_list_node));
  DECL_NAME (prelude_decl)
    = a68_get_mangled_identifier ("_prelude",
				  ctx.module_definition_name);
  DECL_EXTERNAL (prelude_decl) = 0;
  TREE_PUBLIC (prelude_decl) = 1;
  TREE_STATIC (prelude_decl) = 1;

  a68_push_function_range (prelude_decl,
			   void_type_node /* result_type */, true /* top_level */);
  {
    /* Increase sentinel.  */
    a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR,
			       sizetype,
			       sentinel_decl, size_one_node));

    a68_push_stmt_list (NULL);
    {
      a68_push_stmt_list (NULL);
      {
	/* Add calls to preludes of modules in REVELATION_PART.  */
	lower_revelations (revelation_part, ctx, true /* prelude */);
	a68_add_stmt (a68_lower_tree (prelude_enquiry, ctx));
      }
      tree do_prelude = a68_pop_stmt_list ();

      a68_push_stmt_list (M_VOID);
      tree do_nothing = a68_pop_stmt_list ();

      /* Do the prelude work only if sentinel is 1.  */
      a68_add_stmt (fold_build3 (COND_EXPR, void_type_node,
				 fold_build2 (EQ_EXPR, sizetype,
					      sentinel_decl, size_one_node),
				 do_prelude, do_nothing));
    }
    tree prelude_body = a68_pop_stmt_list ();
    a68_pop_function_range (prelude_body);
  }

  /* Create the postlude function.  This is done even if the module definition
     has no postlude in the source code.  */

  location_t postlude_loc = UNKNOWN_LOCATION;
  if (postlude_part != NO_NODE)
    postlude_loc = a68_get_node_location (postlude_part);
  tree postlude_decl = build_decl (postlude_loc,
				   FUNCTION_DECL,
				   NULL_TREE, /* name, set below.  */
				   build_function_type (void_type_node,
							void_list_node));
  DECL_NAME (postlude_decl)
    = a68_get_mangled_identifier ("_postlude",
				  ctx.module_definition_name);
  DECL_EXTERNAL (postlude_decl) = 0;
  TREE_PUBLIC (postlude_decl) = 1;
  TREE_STATIC (postlude_decl) = 1;

  a68_push_function_range (postlude_decl,
			   void_type_node /* result_type */, true /* top_level */);
  {
    /* Decrease sentinel.  */
    a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR,
			       sizetype,
			       sentinel_decl, size_one_node));

    a68_push_stmt_list (NULL);
    {
      a68_push_stmt_list (NULL);
      {
	/* Add calls to postludes of modules in REVELATION_PART.  */
	lower_revelations (revelation_part, ctx, false /* prelude */);
	/* Perhaps the postlude code, if there is one.  */
	NODE_T *postlude_serial = NO_NODE;
	if (postlude_part != NO_NODE)
	  postlude_serial = NEXT_SUB (postlude_part);
	if (postlude_serial != NO_NODE)
	  a68_add_stmt (a68_lower_tree (postlude_serial, ctx));
      }
      tree do_postlude = a68_pop_stmt_list ();

      a68_push_stmt_list (M_VOID);
      tree do_nothing = a68_pop_stmt_list ();

      /* Do the postlude work only if sentinel is 0.  */
      a68_add_stmt (fold_build3 (COND_EXPR, void_type_node,
				 fold_build2 (EQ_EXPR, sizetype,
					      sentinel_decl, size_zero_node),
				 do_postlude, do_nothing));
    }
    tree postlude_body = a68_pop_stmt_list ();
    a68_pop_function_range (postlude_body);
  }

  return NULL_TREE;
}

/* Lower a set of module declarations.

   module declaration : module symbol, defining module, equals symbol, module text ;
                        module_declaration, comma symbol, defining module,
                          equals symbol, module text.

   Each module declaration lowers into the side-effects of emitting prelude and
   postlude global functions, and emitting global declarations for the
   declarations in the module definition prelude.

   This handler always returns NULL_TREE.  */

static tree
lower_module_declaration (NODE_T *p, LOW_CTX_T ctx)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, DEFINING_MODULE_INDICANT))
	{
	  ctx.module_definition_name = NSYMBOL (p);
	  A68_MODULE_DEFINITION_DECLS->truncate (0);
	  vec_alloc (A68_MODULE_DEFINITION_DECLS, 16);
	  lower_module_text (NEXT (NEXT (p)), ctx);
	  for (tree d : A68_MODULE_DEFINITION_DECLS)
	    {
	      if (TREE_CODE (d) == FUNCTION_DECL)
		cgraph_node::finalize_function (d, true);
	      else
		{
		  rest_of_decl_compilation (d, 1, 0);
		  make_decl_rtl (d);
		}
	    }
	}
      else
	lower_module_declaration (SUB (p), ctx);
    }

  return NULL_TREE;
}

/* Lower a prelude packet.

     prelude packet : module declaration.

   This handler always returns NULL_TREE.  */

static tree
lower_prelude_packet (NODE_T *p, LOW_CTX_T ctx)
{
  a68_lower_tree (SUB (p), ctx);
  return NULL_TREE;
}

/* Lower a particular program.

     particular program : label, enclosed clause; enclosed clause.

   This handler always returns NULL_TREE.  */

static tree
lower_particular_program (NODE_T *p, LOW_CTX_T ctx)
{
  /* Create the main function that conforms the particular program.  */
  tree main_decl = build_decl (a68_get_node_location (p),
			       FUNCTION_DECL,
			       get_identifier ("__algol68_main"),
			       build_function_type (void_type_node,
						    void_list_node));
  DECL_EXTERNAL (main_decl) = 0;
  TREE_PUBLIC (main_decl) = 1;
  TREE_STATIC (main_decl) = 1;

  a68_push_function_range (main_decl,
			   void_type_node /* result_type */);

  /* Lower the body of the function.  */
  NODE_T *enclosed_clause = (IS (SUB (p), ENCLOSED_CLAUSE)
			     ? SUB (p) : NEXT (SUB (p)));
  tree body_expr = a68_lower_tree (enclosed_clause, ctx);
  a68_pop_function_range (body_expr);
  return NULL_TREE;
}

/* Lower a packet.

     packet : particular program ; prelude packet.
*/

static tree
lower_packet (NODE_T *p,
	      LOW_CTX_T ctx)
{
  return a68_lower_tree (SUB (p), ctx);
}

/* Lower the given tree P using the given context CTX.  */

tree
a68_lower_tree (NODE_T *p, LOW_CTX_T ctx)
{
#if 0
  for (int i = 0; i < ctx.level; ++i)
    printf (" ");
  printf ("LOWER TREE: %d::%s\n",
  	  NUMBER (p), a68_attribute_name (ATTRIBUTE (p)));
#endif
  ctx.level++;

  tree res = NULL_TREE;

  if (p == NO_NODE)
    gcc_unreachable ();

  switch (ATTRIBUTE (p))
    {
    case PACKET:
      res = lower_packet (p, ctx);
      break;
    case PRELUDE_PACKET:
      res = lower_prelude_packet (p, ctx);
      break;
    case MODULE_DECLARATION:
      res = lower_module_declaration (p, ctx);
      break;
    case MODULE_TEXT:
      res = lower_module_text (p, ctx);
      break;
    case PARTICULAR_PROGRAM:
      res = lower_particular_program (p, ctx);
      break;
      /* Clauses */
    case ENCLOSED_CLAUSE:
      res = a68_lower_enclosed_clause (p, ctx);
      break;
    case CLOSED_CLAUSE:
      res = a68_lower_closed_clause (p, ctx);
      break;
    case ACCESS_CLAUSE:
      res = a68_lower_access_clause (p, ctx);
      break;
    case PARALLEL_CLAUSE:
      res = a68_lower_parallel_clause (p, ctx);
      break;
    case COLLATERAL_CLAUSE:
      res = a68_lower_collateral_clause (p, ctx);
      break;
    case UNIT_LIST:
      res = a68_lower_unit_list (p, ctx);
      break;
    case CONDITIONAL_CLAUSE:
      res = a68_lower_conditional_clause (p, ctx);
      break;
    case ENQUIRY_CLAUSE:
      res = a68_lower_enquiry_clause (p, ctx);
      break;
    case CASE_CLAUSE:
      res = a68_lower_case_clause (p, ctx);
      break;
    case CONFORMITY_CLAUSE:
      res = a68_lower_conformity_clause (p, ctx);
      break;
    case LOOP_CLAUSE:
      res = a68_lower_loop_clause (p, ctx);
      break;
    case SERIAL_CLAUSE:
      res = a68_lower_serial_clause (p, ctx);
      break;
    case INITIALISER_SERIES:
      res = a68_lower_initialiser_series (p, ctx);
      break;
    case EXIT_SYMBOL:
      res = a68_lower_completer (p, ctx);
      break;
    case LABELED_UNIT:
      res = a68_lower_labeled_unit (p, ctx);
      break;
    case LABEL:
      res = a68_lower_label (p, ctx);
      break;
      /* Declarations.  */
    case DECLARATION_LIST:
      res = a68_lower_declaration_list (p, ctx);
      break;
    case DECLARER:
      res = a68_lower_declarer (p, ctx);
      break;
    case IDENTITY_DECLARATION:
      res = a68_lower_identity_declaration (p, ctx);
      break;
    case VARIABLE_DECLARATION:
      res = a68_lower_variable_declaration (p, ctx);
      break;
    case PROCEDURE_DECLARATION:
      res = a68_lower_procedure_declaration (p, ctx);
      break;
    case PROCEDURE_VARIABLE_DECLARATION:
      res = a68_lower_procedure_variable_declaration (p, ctx);
      break;
    case PRIORITY_DECLARATION:
      res = a68_lower_priority_declaration (p, ctx);
      break;
    case BRIEF_OPERATOR_DECLARATION:
      res = a68_lower_brief_operator_declaration (p, ctx);
      break;
    case OPERATOR_DECLARATION:
      res = a68_lower_operator_declaration (p, ctx);
      break;
    case MODE_DECLARATION:
      res = a68_lower_mode_declaration (p, ctx);
      break;
      /* Units. */
    case UNIT:
      res = a68_lower_unit (p, ctx);
      break;
    case ROUTINE_TEXT:
      res = a68_lower_routine_text (p, ctx);
      break;
    case ASSIGNATION:
      res = a68_lower_assignation (p, ctx);
      break;
    case TERTIARY:
      res = a68_lower_tertiary (p, ctx);
      break;
    case MONADIC_FORMULA:
      res = a68_lower_monadic_formula (p, ctx);
      break;
    case FORMULA:
      res = a68_lower_formula (p, ctx);
      break;
    case SECONDARY:
      res = a68_lower_secondary (p, ctx);
      break;
    case SLICE:
      res = a68_lower_slice (p, ctx);
      break;
    case SELECTION:
      res = a68_lower_selection (p, ctx);
      break;
    case PRIMARY:
      res = a68_lower_primary (p, ctx);
      break;
    case GENERATOR:
      res = a68_lower_generator (p, ctx);
      break;
    case CALL:
      res = a68_lower_call (p, ctx);
      break;
    case CAST:
      res = a68_lower_cast (p, ctx);
      break;
    case AND_FUNCTION:
    case OR_FUNCTION:
      res = a68_lower_logic_function (p, ctx);
      break;
    case IDENTITY_RELATION:
      res = a68_lower_identity_relation (p, ctx);
      break;
    case EMPTY_SYMBOL:
      res = a68_lower_empty (p, ctx);
      break;
    case NIHIL:
      res = a68_lower_nihil (p, ctx);
      break;
    case SKIP:
      res = a68_lower_skip (p, ctx);
      break;
    case DENOTATION:
      res = a68_lower_denotation (p, ctx);
      break;
    case IDENTIFIER:
      res = a68_lower_identifier (p, ctx);
      break;
      /* Coercions.  */
    case ROWING:
      res = a68_lower_rowing (p, ctx);
      break;
    case WIDENING:
      res = a68_lower_widening (p, ctx);
      break;
    case DEPROCEDURING:
      res = a68_lower_deproceduring (p, ctx);
      break;
    case PROCEDURING:
      res = a68_lower_proceduring (p, ctx);
      break;
    case VOIDING:
      res = a68_lower_voiding (p, ctx);
      break;
    case DEREFERENCING:
      res = a68_lower_dereferencing (p, ctx);
      break;
      /* Others. */
    case UNITING:
      res = a68_lower_uniting (p, ctx);
      break;
    case JUMP:
      res = a68_lower_jump (p, ctx);
      break;
    case PARAMETER:
      res = a68_lower_parameter (p, ctx);
      break;
    case PARAMETER_LIST:
      res = a68_lower_parameter_list (p, ctx);
      break;
    case PARAMETER_PACK:
      res = a68_lower_parameter_pack (p, ctx);
      break;
    case OPERATOR:
      res = a68_lower_operator (p, ctx);
      break;
    case ASSERTION:
      res = a68_lower_assertion (p, ctx);
      break;
    case STOP:
      res = NULL_TREE;
      break;
    default:
      fatal_error (a68_get_node_location (p), "cannot lower node %s",
		   a68_attribute_name (ATTRIBUTE (p)));
      gcc_unreachable ();
      break;
    }

  return res;
}

/* Lower an Algol 68 complete parse tree to a GENERIC tree.  */

tree
a68_lower_top_tree (NODE_T *p)
{
  LOW_CTX_T top_ctx;

  top_ctx.declarer = NULL;
  top_ctx.proc_decl_identifier = NO_NODE;
  top_ctx.proc_decl_operator = false;
  top_ctx.level = 0;
  top_ctx.module_definition_name = NULL;
  vec_alloc (A68_MODULE_DEFINITION_DECLS, 16);
  return a68_lower_tree (p, top_ctx);
}
