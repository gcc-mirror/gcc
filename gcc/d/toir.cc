/* toir.cc -- Lower D frontend statements to GCC trees.
   Copyright (C) 2006-2023 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/aggregate.h"
#include "dmd/declaration.h"
#include "dmd/expression.h"
#include "dmd/identifier.h"
#include "dmd/init.h"
#include "dmd/statement.h"

#include "tree.h"
#include "tree-iterator.h"
#include "options.h"
#include "stmt.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "function.h"
#include "toplev.h"

#include "d-tree.h"


/* Update data for defined and undefined labels when leaving a scope.  */

bool
pop_binding_label (Statement * const &, d_label_entry *ent, binding_level *bl)
{
  binding_level *obl = bl->level_chain;

  if (ent->level == bl)
    {
      if (bl->kind == level_try)
	ent->in_try_scope = true;
      else if (bl->kind == level_catch)
	ent->in_catch_scope = true;

      ent->level = obl;
    }
  else if (ent->fwdrefs)
    {
      for (d_label_use_entry *ref = ent->fwdrefs; ref; ref = ref->next)
	ref->level = obl;
    }

  return true;
}

/* At the end of a function, all labels declared within the function
   go out of scope.  Queue them in LABELS.  */

bool
pop_label (Statement * const &, d_label_entry *ent, vec <tree> &labels)
{
  if (!ent->bc_label)
    {
      /* Put the labels into the "variables" of the top-level block,
	 so debugger can see them.  */
      if (DECL_NAME (ent->label))
	{
	  gcc_assert (DECL_INITIAL (ent->label) != NULL_TREE);
	  labels.safe_push (ent->label);
	}
    }

  return true;
}

/* The D front-end does not use the 'binding level' system for a symbol table,
   however it has been the goto structure for tracking code flow.
   Primarily it is only needed to get debugging information for local variables
   and otherwise support the back-end.  */

void
push_binding_level (level_kind kind)
{
  /* Add it to the front of currently active scopes stack.  */
  binding_level *new_level = ggc_cleared_alloc <binding_level> ();
  new_level->level_chain = current_binding_level;
  new_level->kind = kind;

  current_binding_level = new_level;
}

static int
cmp_labels (const void *p1, const void *p2)
{
  const tree *l1 = (const tree *) p1;
  const tree *l2 = (const tree *) p2;
  return DECL_UID (*l1) - DECL_UID (*l2);
}

tree
pop_binding_level (void)
{
  binding_level *level = current_binding_level;
  current_binding_level = level->level_chain;

  tree block = make_node (BLOCK);
  BLOCK_VARS (block) = level->names;
  BLOCK_SUBBLOCKS (block) = level->blocks;

  /* In each subblock, record that this is its superior.  */
  for (tree t = level->blocks; t; t = BLOCK_CHAIN (t))
    BLOCK_SUPERCONTEXT (t) = block;

  if (level->kind == level_function)
    {
      /* Dispose of the block that we just made inside some higher level.  */
      DECL_INITIAL (current_function_decl) = block;
      BLOCK_SUPERCONTEXT (block) = current_function_decl;

      /* Pop all the labels declared in the function.  */
      if (d_function_chain->labels)
	{
	  auto_vec <tree> labels;
	  d_function_chain->labels->traverse <vec <tree> &,
					      &pop_label> (labels);
	  d_function_chain->labels->empty ();
	  labels.qsort (cmp_labels);
	  for (unsigned i = 0; i < labels.length (); ++i)
	    {
	      DECL_CHAIN (labels[i]) = BLOCK_VARS (block);
	      BLOCK_VARS (block) = labels[i];
	    }
	}
    }
  else
    {
      /* Any uses of undefined labels, and any defined labels, now operate
	 under constraints of next binding contour.  */
      if (d_function_chain && d_function_chain->labels)
	{
	  language_function *f = d_function_chain;
	  f->labels->traverse <binding_level *, &pop_binding_label> (level);
	}

      current_binding_level->blocks
	= block_chainon (current_binding_level->blocks, block);
    }

  TREE_USED (block) = 1;
  return block;
}

/* Create an empty statement tree rooted at T.  */

void
push_stmt_list (void)
{
  tree t = alloc_stmt_list ();
  vec_safe_push (d_function_chain->stmt_list, t);
  d_keep (t);
}

/* Finish the statement tree rooted at T.  */

tree
pop_stmt_list (void)
{
  tree t = d_function_chain->stmt_list->pop ();

  /* If the statement list is completely empty, just return it.  This is just
     as good as build_empty_stmt, with the advantage that statement lists
     are merged when they are appended to one another.  So using the
     STATEMENT_LIST avoids pathological buildup of EMPTY_STMT_P statements.  */
  if (TREE_SIDE_EFFECTS (t))
    {
      /* If the statement list contained exactly one statement, then extract
	 it immediately.  */
      tree_stmt_iterator i = tsi_start (t);

      if (tsi_one_before_end_p (i))
	{
	  tree u = tsi_stmt (i);
	  tsi_delink (&i);
	  free_stmt_list (t);
	  t = u;
	}
    }

  return t;
}

/* T is an expression statement.  Add it to the statement-tree.  */

void
add_stmt (tree t)
{
  /* Ignore (void) 0; expression statements received from the frontend.
     Likewise void_node is used when contracts become nops in release code.  */
  if (t == void_node || IS_EMPTY_STMT (t))
    return;

  /* At this point, we no longer care about the value of expressions,
     so if there's no side-effects, then don't add it.  */
  if (!TREE_SIDE_EFFECTS (t))
    return;

  if (TREE_CODE (t) == COMPOUND_EXPR)
    {
      /* Push out each comma expressions as separate statements.  */
      add_stmt (TREE_OPERAND (t, 0));
      add_stmt (TREE_OPERAND (t, 1));
    }
  else
    {
      /* Force the type to be void so we don't need to create a temporary
	 variable to hold the inner expression.  */
      if (TREE_CODE (t) == CLEANUP_POINT_EXPR)
	TREE_TYPE (t) = void_type_node;

      /* Append the expression to the statement list.
	 Make sure it has a proper location.  */
      if (EXPR_P (t) && !EXPR_HAS_LOCATION (t))
	SET_EXPR_LOCATION (t, input_location);

      tree stmt_list = d_function_chain->stmt_list->last ();
      append_to_statement_list_force (t, &stmt_list);
    }
}

/* Implements the visitor interface to build the GCC trees of all Statement
   AST classes emitted from the D Front-end.
   All visit methods accept one parameter S, which holds the frontend AST
   of the statement to compile.  They also don't return any value, instead
   generated code are pushed to add_stmt(), which appends them to the
   statement list in the current_binding_level.  */

class IRVisitor : public Visitor
{
  using Visitor::visit;

  FuncDeclaration *func_;

  /* Stack of labels which are targets for "break" and "continue",
     linked through TREE_CHAIN.  */
  tree break_label_;
  tree continue_label_;

public:
  IRVisitor (FuncDeclaration *fd)
  {
    this->func_ = fd;
    this->break_label_ = NULL_TREE;
    this->continue_label_ = NULL_TREE;
  }

  /* Helper for generating code for the statement AST class S.
     Sets up the location of the statement before lowering.  */

  void build_stmt (Statement *s)
  {
    location_t saved_location = input_location;
    input_location = make_location_t (s->loc);
    s->accept (this);
    input_location = saved_location;
  }

  /* Start a new scope for a KIND statement.
     Each user-declared variable will have a binding contour that begins
     where the variable is declared and ends at its containing scope.  */

  void start_scope (level_kind kind)
  {
    push_binding_level (kind);
    push_stmt_list ();
  }

  /* Leave scope pushed by start_scope, returning a new bind_expr if
     any variables where declared in the scope.  */

  tree end_scope (void)
  {
    tree block = pop_binding_level ();
    tree body = pop_stmt_list ();

    if (!BLOCK_VARS (block))
      return body;

    tree bind = build3 (BIND_EXPR, void_type_node,
			BLOCK_VARS (block), body, block);
    TREE_SIDE_EFFECTS (bind) = 1;
    return bind;
  }

  /* Like end_scope, but also push it into the outer statement-tree.  */

  void finish_scope (void)
  {
    tree scope = this->end_scope ();
    add_stmt (scope);
  }

  /* Return TRUE if IDENT is the current function return label.  */

  bool is_return_label (Identifier *ident)
  {
    if (this->func_->returnLabel)
      return this->func_->returnLabel->ident == ident;

    return false;
  }

  /* Define a label, specifying the location in the source file.
     Return the LABEL_DECL node for the label.  */

  tree define_label (Statement *s, Identifier *ident = NULL)
  {
    tree label = this->lookup_label (s, ident);
    gcc_assert (DECL_INITIAL (label) == NULL_TREE);

    d_label_entry *ent = d_function_chain->labels->get (s);
    gcc_assert (ent != NULL);

    /* Mark label as having been defined.  */
    DECL_INITIAL (label) = error_mark_node;

    ent->level = current_binding_level;

    for (d_label_use_entry *ref = ent->fwdrefs; ref ; ref = ref->next)
      this->check_previous_goto (ent->statement, ref);
    ent->fwdrefs = NULL;

    return label;
  }

  /* Emit a LABEL expression.  */

  void do_label (tree label)
  {
    /* Don't write out label unless it is marked as used by the frontend.
       This makes auto-vectorization possible in conditional loops.
       The only excemption to this is in the LabelStatement visitor,
       in which all computed labels are marked regardless.  */
    if (TREE_USED (label))
      add_stmt (build1 (LABEL_EXPR, void_type_node, label));
  }

  /* Emit a goto expression to LABEL.  */

  void do_jump (tree label)
  {
    add_stmt (fold_build1 (GOTO_EXPR, void_type_node, label));
    TREE_USED (label) = 1;
  }

  /* Check that a new jump at statement scope FROM to a label declared in
     statement scope TO is valid.  */

  void check_goto (Statement *from, Statement *to)
  {
    d_label_entry *ent = d_function_chain->labels->get (to);
    gcc_assert (ent != NULL);

    /* If the label hasn't been defined yet, defer checking.  */
    if (!DECL_INITIAL (ent->label))
      {
	d_label_use_entry *fwdref = ggc_alloc <d_label_use_entry> ();
	fwdref->level = current_binding_level;
	fwdref->statement = from;
	fwdref->next = ent->fwdrefs;
	ent->fwdrefs = fwdref;
	return;
      }

    if (ent->in_try_scope)
      error_at (make_location_t (from->loc),
		"cannot %<goto%> into %<try%> block");
    else if (ent->in_catch_scope)
      error_at (make_location_t (from->loc),
		"cannot %<goto%> into %<catch%> block");
  }

  /* Check that a previously seen jump to a newly defined label is valid.
     S is the label statement; FWDREF is the jump context.  This is called
     for both user-defined and case labels.  */

  void check_previous_goto (Statement *s, d_label_use_entry *fwdref)
  {
    for (binding_level *b = current_binding_level; b ; b = b->level_chain)
      {
	if (b == fwdref->level)
	  break;

	if (b->kind == level_try || b->kind == level_catch)
	  {
	    location_t location;

	    if (s->isLabelStatement ())
	      {
		location = make_location_t (fwdref->statement->loc);
		if (b->kind == level_try)
		  error_at (location, "cannot %<goto%> into %<try%> block");
		else
		  error_at (location, "cannot %<goto%> into %<catch%> block");
	      }
	    else
	      gcc_unreachable ();
	  }
      }
  }

  /* Get or build LABEL_DECL using the IDENT and statement block S given.  */

  tree lookup_label (Statement *s, Identifier *ident = NULL)
  {
    /* You can't use labels at global scope.  */
    if (d_function_chain == NULL)
      {
	error ("label %s referenced outside of any function",
	       ident ? ident->toChars () : "(unnamed)");
	return NULL_TREE;
      }

    /* Create the label htab for the function on demand.  */
    if (!d_function_chain->labels)
      {
	d_function_chain->labels
	  = hash_map <Statement *, d_label_entry>::create_ggc (13);
      }

    d_label_entry *ent = d_function_chain->labels->get (s);
    if (ent != NULL)
      return ent->label;
    else
      {
	tree name = ident ? get_identifier (ident->toChars ()) : NULL_TREE;
	tree decl = build_decl (make_location_t (s->loc), LABEL_DECL,
				name, void_type_node);
	DECL_CONTEXT (decl) = current_function_decl;
	DECL_MODE (decl) = VOIDmode;

	/* Create new empty slot.  */
	ent = ggc_cleared_alloc <d_label_entry> ();
	ent->statement = s;
	ent->label = decl;

	bool existed = d_function_chain->labels->put (s, *ent);
	gcc_assert (!existed);

	return decl;
      }
  }

  /* Get the LABEL_DECL to represent a break or continue for the
     statement S given.  BC indicates which.  */

  tree lookup_bc_label (Statement *s, bc_kind bc)
  {
    tree vec = this->lookup_label (s);

    /* The break and continue labels are put into a TREE_VEC.  */
    if (TREE_CODE (vec) == LABEL_DECL)
      {
	d_label_entry *ent = d_function_chain->labels->get (s);
	gcc_assert (ent != NULL);

	vec = make_tree_vec (2);
	TREE_VEC_ELT (vec, bc_break) = ent->label;

	/* Build the continue label.  */
	tree label = build_decl (make_location_t (s->loc), LABEL_DECL,
				 NULL_TREE, void_type_node);
	DECL_CONTEXT (label) = current_function_decl;
	DECL_MODE (label) = VOIDmode;
	TREE_VEC_ELT (vec, bc_continue) = label;

	ent->label = vec;
	ent->bc_label = true;
      }

    return TREE_VEC_ELT (vec, bc);
  }

  /* Set and return the current break label for the current block.  */

  tree push_break_label (Statement *s)
  {
    tree label = this->lookup_bc_label (s->getRelatedLabeled (), bc_break);
    DECL_CHAIN (label) = this->break_label_;
    this->break_label_ = label;
    return label;
  }

  /* Finish with the current break label.  */

  void pop_break_label (tree label)
  {
    gcc_assert (this->break_label_ == label);
    this->break_label_ = DECL_CHAIN (this->break_label_);
    this->do_label (label);
  }

  /* Set and return the continue label for the current block.  */

  tree push_continue_label (Statement *s)
  {
    tree label = this->lookup_bc_label (s->getRelatedLabeled (), bc_continue);
    DECL_CHAIN (label) = this->continue_label_;
    this->continue_label_ = label;
    return label;
  }

  /* Finish with the current continue label.  */

  void pop_continue_label (tree label)
  {
    gcc_assert (this->continue_label_ == label);
    this->continue_label_ = DECL_CHAIN (this->continue_label_);
    this->do_label (label);
  }

  /* Generate and set a new continue label for the current unrolled loop.  */

  void push_unrolled_continue_label (UnrolledLoopStatement *s)
  {
    this->push_continue_label (s);
  }

  /* Finish with the continue label for the unrolled loop.  */

  void pop_unrolled_continue_label (UnrolledLoopStatement *s)
  {
    Statement *stmt = s->getRelatedLabeled ();
    d_label_entry *ent = d_function_chain->labels->get (stmt);
    gcc_assert (ent != NULL && ent->bc_label == true);

    this->pop_continue_label (TREE_VEC_ELT (ent->label, bc_continue));

    /* Remove the continue label from the label htab, as a new one must be
       inserted at the end of every unrolled loop.  */
    ent->label = TREE_VEC_ELT (ent->label, bc_break);
  }

  /* Visitor interfaces.  */


  /* This should be overridden by each statement class.  */

  void visit (Statement *) final override
  {
    gcc_unreachable ();
  }

  /* The frontend lowers `scope (exit/failure/success)' statements as
     try/catch/finally.  At this point, this statement is just an empty
     placeholder.  Maybe the frontend shouldn't leak these.  */

  void visit (ScopeGuardStatement *) final override
  {
  }

  /* If statements provide simple conditional execution of statements.  */

  void visit (IfStatement *s) final override
  {
    this->start_scope (level_cond);

    /* Build the outer `if' condition, which may produce temporaries
       requiring scope destruction.  */
    tree ifcond = convert_for_condition (build_expr_dtor (s->condition),
					 s->condition->type);
    tree ifbody = void_node;
    tree elsebody = void_node;

    /* Build the `then' branch, don't do code generation when the condition
       is `if (__ctfe)', as that is always false at run-time.  */
    if (s->ifbody && !s->isIfCtfeBlock ())
      {
	push_stmt_list ();
	this->build_stmt (s->ifbody);
	ifbody = pop_stmt_list ();
      }

    /* Now build the `else' branch, which may have nested `else if' parts.  */
    if (s->elsebody)
      {
	push_stmt_list ();
	this->build_stmt (s->elsebody);
	elsebody = pop_stmt_list ();
      }

    /* Wrap up our constructed if condition into a COND_EXPR.  */
    tree cond = build_vcondition (ifcond, ifbody, elsebody);
    add_stmt (cond);

    /* Finish the if-then scope.  */
    this->finish_scope ();
  }

  /* Should there be any `pragma (...)' statements requiring code generation,
     here would be the place to do it.  For now, all pragmas are handled
     by the frontend.  */

  void visit (PragmaStatement *) final override
  {
  }

  /* The frontend lowers `while (...)' statements as `for (...)' loops.
     This visitor is not strictly required other than to enforce that
     these kinds of statements never reach here.  */

  void visit (WhileStatement *) final override
  {
    gcc_unreachable ();
  }

  /* Do while statments implement simple loops.  The body is executed, then
     the condition is evaluated.  */

  void visit (DoStatement *s) final override
  {
    tree lbreak = this->push_break_label (s);

    this->start_scope (level_loop);
    if (s->_body)
      {
	tree lcontinue = this->push_continue_label (s);
	this->build_stmt (s->_body);
	this->pop_continue_label (lcontinue);
      }

    /* Build the outer `while' condition, which may produce temporaries
       requiring scope destruction.  */
    tree exitcond = convert_for_condition (build_expr_dtor (s->condition),
					   s->condition->type);
    add_stmt (build_vcondition (exitcond, void_node,
				build1 (GOTO_EXPR, void_type_node, lbreak)));
    TREE_USED (lbreak) = 1;

    tree body = this->end_scope ();
    add_stmt (build1 (LOOP_EXPR, void_type_node, body));

    this->pop_break_label (lbreak);
  }

  /* For statements implement loops with initialization, test, and
     increment clauses.  */

  void visit (ForStatement *s) final override
  {
    tree lbreak = this->push_break_label (s);
    this->start_scope (level_loop);

    if (s->_init)
      this->build_stmt (s->_init);

    if (s->condition)
      {
	tree exitcond = convert_for_condition (build_expr_dtor (s->condition),
					       s->condition->type);
	add_stmt (build_vcondition (exitcond, void_node,
				    build1 (GOTO_EXPR, void_type_node,
					    lbreak)));
	TREE_USED (lbreak) = 1;
      }

    if (s->_body)
      {
	tree lcontinue = this->push_continue_label (s);
	this->build_stmt (s->_body);
	this->pop_continue_label (lcontinue);
      }

    if (s->increment)
      {
	/* Force side effects?  */
	add_stmt (build_expr_dtor (s->increment));
      }

    tree body = this->end_scope ();
    add_stmt (build1 (LOOP_EXPR, void_type_node, body));

    this->pop_break_label (lbreak);
  }

  /* The frontend lowers `foreach (...)' statements as `for (...)' loops.
     This visitor is not strictly required other than to enforce that
     these kinds of statements never reach here.  */

  void visit (ForeachStatement *) final override
  {
    gcc_unreachable ();
  }

  /* The frontend lowers `foreach (...; [x..y])' statements as `for (...)'
     loops.  This visitor is not strictly required other than to enforce that
     these kinds of statements never reach here.  */

  void visit (ForeachRangeStatement *) final override
  {
    gcc_unreachable ();
  }

  /* Jump to the associated exit label for the current loop.  If IDENT
     for the Statement is not null, then the label is user defined.  */

  void visit (BreakStatement *s) final override
  {
    if (s->ident)
      {
	/* The break label may actually be some levels up.
	   eg: on a try/finally wrapping a loop.  */
	LabelDsymbol *sym = this->func_->searchLabel (s->ident, s->loc);
	LabelStatement *label = sym->statement;
	gcc_assert (label != NULL);
	Statement *stmt = label->statement->getRelatedLabeled ();
	this->do_jump (this->lookup_bc_label (stmt, bc_break));
      }
    else
      this->do_jump (this->break_label_);
  }

  /* Jump to the associated continue label for the current loop.  If IDENT
     for the Statement is not null, then the label is user defined.  */

  void visit (ContinueStatement *s) final override
  {
    if (s->ident)
      {
	LabelDsymbol *sym = this->func_->searchLabel (s->ident, s->loc);
	LabelStatement *label = sym->statement;
	gcc_assert (label != NULL);
	this->do_jump (this->lookup_bc_label (label->statement,
					      bc_continue));
      }
    else
      this->do_jump (this->continue_label_);
  }

  /* A goto statement jumps to the statement identified by the given label.  */

  void visit (GotoStatement *s) final override
  {
    gcc_assert (s->label->statement != NULL);
    gcc_assert (s->tf == s->label->statement->tf);

    /* If no label found, there was an error.  */
    tree label = this->lookup_label (s->label->statement, s->label->ident);
    this->do_jump (label);

    /* Need to error if the goto is jumping into a try or catch block.  */
    this->check_goto (s, s->label->statement);
  }

  /* Statements can be labeled.  A label is an identifier that precedes
     a statement.  */

  void visit (LabelStatement *s) final override
  {
    LabelDsymbol *sym;

    if (this->is_return_label (s->ident))
      sym = this->func_->returnLabel;
    else
      sym = this->func_->searchLabel (s->ident, s->loc);

    /* If no label found, there was an error.  */
    tree label = this->define_label (sym->statement, sym->ident);
    TREE_USED (label) = 1;

    this->do_label (label);

    if (this->is_return_label (s->ident) && this->func_->fensure != NULL)
      this->build_stmt (this->func_->fensure);
    else if (s->statement)
      this->build_stmt (s->statement);
  }

  /* A switch statement goes to one of a collection of case statements
     depending on the value of the switch expression.  */

  void visit (SwitchStatement *s) final override
  {
    this->start_scope (level_switch);
    tree lbreak = this->push_break_label (s);

    tree condition = build_expr_dtor (s->condition);
    Type *condtype = s->condition->type->toBasetype ();

    /* A switch statement on a string gets turned into a library call.
       It is not lowered during codegen.  */
    if (!condtype->isscalar ())
      {
	error ("cannot handle switch condition of type %s",
	       condtype->toChars ());
	gcc_unreachable ();
      }

    condition = fold (condition);

    /* Build LABEL_DECLs now so they can be refered to by goto case.
       Also checking the jump from the switch to the label is allowed.  */
    if (s->cases)
      {
	for (size_t i = 0; i < s->cases->length; i++)
	  {
	    CaseStatement *cs = (*s->cases)[i];
	    tree caselabel = this->lookup_label (cs);

	    /* Write cases as a series of if-then-else blocks.
	       if (condition == case)
		 goto caselabel;  */
	    if (s->hasVars)
	      {
		tree ifcase = build2 (EQ_EXPR, build_ctype (condtype),
				      condition, build_expr_dtor (cs->exp));
		tree ifbody = fold_build1 (GOTO_EXPR, void_type_node,
					   caselabel);
		tree cond = build_vcondition (ifcase, ifbody, void_node);
		TREE_USED (caselabel) = 1;
		LABEL_VARIABLE_CASE (caselabel) = 1;
		add_stmt (cond);
	      }

	    this->check_goto (s, cs);
	  }

	if (s->sdefault)
	  {
	    tree defaultlabel = this->lookup_label (s->sdefault);

	    /* The default label is the last `else' block.  */
	    if (s->hasVars)
	      {
		this->do_jump (defaultlabel);
		LABEL_VARIABLE_CASE (defaultlabel) = 1;
	      }

	    this->check_goto (s, s->sdefault);
	  }
      }

    /* Switch body goes in its own statement list.  */
    push_stmt_list ();
    if (s->_body)
      this->build_stmt (s->_body);

    tree casebody = pop_stmt_list ();

    /* Wrap up constructed body into a switch_expr, unless it was
       converted to an if-then-else expression.  */
    if (s->hasVars)
      add_stmt (casebody);
    else
      {
	tree switchexpr = build2 (SWITCH_EXPR, TREE_TYPE (condition),
				  condition, casebody);
	add_stmt (switchexpr);
	SWITCH_ALL_CASES_P (switchexpr) = 1;
      }

    SWITCH_BREAK_LABEL_P (lbreak) = 1;

    /* If the switch had any `break' statements, emit the label now.  */
    this->pop_break_label (lbreak);
    this->finish_scope ();
  }

  /* Declare the case label associated with the current SwitchStatement.  */

  void visit (CaseStatement *s) final override
  {
    /* Emit the case label.  */
    tree label = this->define_label (s);

    if (LABEL_VARIABLE_CASE (label))
      this->do_label (label);
    else
      {
	tree casevalue;
	if (s->exp->type->isscalar ())
	  casevalue = build_expr (s->exp);
	else
	  casevalue = build_integer_cst (s->index, build_ctype (Type::tint32));

	tree caselabel = build_case_label (casevalue, NULL_TREE, label);
	add_stmt (caselabel);
      }

    /* Now do the body.  */
    if (s->statement)
      this->build_stmt (s->statement);
  }

  /* Declare the default label associated with the current SwitchStatement.  */

  void visit (DefaultStatement *s) final override
  {
    /* Emit the default case label.  */
    tree label = this->define_label (s);

    if (LABEL_VARIABLE_CASE (label))
      this->do_label (label);
    else
      {
	tree caselabel = build_case_label (NULL_TREE, NULL_TREE, label);
	add_stmt (caselabel);
      }

    /* Now do the body.  */
    if (s->statement)
      this->build_stmt (s->statement);
  }

  /* Implements `goto default' by jumping to the label associated with
     the DefaultStatement in a switch block.  */

  void visit (GotoDefaultStatement *s) final override
  {
    tree label = this->lookup_label (s->sw->sdefault);
    this->do_jump (label);
  }

  /* Implements `goto case' by jumping to the label associated with the
     CaseStatement in a switch block.  */

  void visit (GotoCaseStatement *s) final override
  {
    tree label = this->lookup_label (s->cs);
    this->do_jump (label);
  }

  /* Throw a SwitchError exception, called when a switch statement has
     no DefaultStatement, yet none of the cases match.  */

  void visit (SwitchErrorStatement *s) final override
  {
    /* A throw SwitchError statement gets turned into a library call.
       The call is wrapped in the enclosed expression.  */
    gcc_assert (s->exp != NULL);
    add_stmt (build_expr (s->exp));
  }

  /* A return statement exits the current function and supplies its return
     value, if the return type is not void.  */

  void visit (ReturnStatement *s) final override
  {
    if (s->exp == NULL || s->exp->type->toBasetype ()->ty == TY::Tvoid)
      {
	/* Return has no value.  */
	add_stmt (return_expr (NULL_TREE));
	return;
      }

    TypeFunction *tf = this->func_->type->toTypeFunction ();
    Type *type = this->func_->tintro != NULL
      ? this->func_->tintro->nextOf () : tf->nextOf ();

    if ((this->func_->isMain () || this->func_->isCMain ())
	&& type->toBasetype ()->ty == TY::Tvoid)
      type = Type::tint32;

    if (this->func_->shidden)
      {
	/* Returning by hidden reference, store the result into the retval decl.
	   The result returned then becomes the retval reference itself.  */
	tree decl = DECL_RESULT (get_symbol_decl (this->func_));
	gcc_assert (!tf->isref ());

	/* If returning via NRVO, just refer to the DECL_RESULT; this differs
	   from using NULL_TREE in that it indicates that we care about the
	   value of the DECL_RESULT.  */
	if (this->func_->isNRVO () && this->func_->nrvo_var)
	  {
	    add_stmt (return_expr (decl));
	    return;
	  }

	/* Detect a call to a constructor function, or if returning a struct
	   literal, write result directly into the return value.  */
	StructLiteralExp *sle = NULL;
	bool using_rvo_p = false;

	if (DotVarExp *dve = (s->exp->isCallExp ()
			      ? s->exp->isCallExp ()->e1->isDotVarExp ()
			      : NULL))
	  {
	    if (dve->var->isCtorDeclaration ())
	      {
		if (CommaExp *ce = dve->e1->isCommaExp ())
		  {
		    /* Temporary initialized inside a return expression, and
		       used as the return value.  Replace it with the hidden
			reference to allow RVO return.  */
		    DeclarationExp *de = ce->e1->isDeclarationExp ();
		    VarExp *ve = ce->e2->isVarExp ();
		    if (de != NULL && ve != NULL
			&& ve->var == de->declaration
			&& ve->var->storage_class & STCtemp)
		      {
			tree var = get_symbol_decl (ve->var);
			TREE_ADDRESSABLE (var) = 1;
			SET_DECL_VALUE_EXPR (var, decl);
			DECL_HAS_VALUE_EXPR_P (var) = 1;
			SET_DECL_LANG_NRVO (var, this->func_->shidden);
			using_rvo_p = true;
		      }
		  }
		else
		  sle = dve->e1->isStructLiteralExp ();
	      }
	  }
	else
	  sle = s->exp->isStructLiteralExp ();

	if (sle != NULL)
	  {
	    StructDeclaration *sd = type->baseElemOf ()->isTypeStruct ()->sym;
	    sle->sym = build_address (this->func_->shidden);
	    using_rvo_p = true;

	    /* Fill any alignment holes in the return slot using memset.  */
	    if (!identity_compare_p (sd) || sd->isUnionDeclaration ())
	      add_stmt (build_memset_call (this->func_->shidden));
	  }

	if (using_rvo_p == true)
	  {
	    /* Generate: (expr, return <retval>);  */
	    add_stmt (build_expr_dtor (s->exp));
	  }
	else
	  {
	    /* Generate: (<retval> = expr, return <retval>);  */
	    tree expr = build_expr_dtor (s->exp);
	    tree init = stabilize_expr (&expr);
	    expr = convert_for_rvalue (expr, s->exp->type, type);
	    expr = build_assign (INIT_EXPR, this->func_->shidden, expr);
	    add_stmt (compound_expr (init, expr));
	  }

	add_stmt (return_expr (decl));
      }
    else if (tf->next->ty == TY::Tnoreturn)
      {
	/* Returning an expression that has no value, but has a side effect
	   that should never return.  */
	add_stmt (build_expr_dtor (s->exp));
	add_stmt (return_expr (NULL_TREE));
      }
    else
      {
	/* Convert for initializing the DECL_RESULT.  */
	add_stmt (build_return_dtor (s->exp, type, tf));
      }
  }

  /* Evaluate the enclosed expression, and add it to the statement list.  */

  void visit (ExpStatement *s) final override
  {
    if (s->exp)
      {
	/* Expression may produce temporaries requiring scope destruction.  */
	tree exp = build_expr_dtor (s->exp);
	add_stmt (exp);
      }
  }

  /* Evaluate all enclosed statements.  */

  void visit (CompoundStatement *s) final override
  {
    if (s->statements == NULL)
      return;

    for (size_t i = 0; i < s->statements->length; i++)
      {
	Statement *statement = (*s->statements)[i];

	if (statement != NULL)
	  this->build_stmt (statement);
      }
  }

  /* The frontend lowers `foreach (Tuple!(...))' statements as an unrolled loop.
     These are compiled down as a `do ... while (0)', where each unrolled loop
     is nested inside and given their own continue label to jump to.  */

  void visit (UnrolledLoopStatement *s) final override
  {
    if (s->statements == NULL)
      return;

    tree lbreak = this->push_break_label (s);
    this->start_scope (level_loop);

    for (size_t i = 0; i < s->statements->length; i++)
      {
	Statement *statement = (*s->statements)[i];

	if (statement != NULL)
	  {
	    this->push_unrolled_continue_label (s);
	    this->build_stmt (statement);
	    this->pop_unrolled_continue_label (s);
	  }
      }

    this->do_jump (this->break_label_);

    tree body = this->end_scope ();
    add_stmt (build1 (LOOP_EXPR, void_type_node, body));

    this->pop_break_label (lbreak);
  }

  /* Start a new scope and visit all nested statements, wrapping
     them up into a BIND_EXPR at the end of the scope.  */

  void visit (ScopeStatement *s) final override
  {
    if (s->statement == NULL)
      return;

    this->start_scope (level_block);
    this->build_stmt (s->statement);
    this->finish_scope ();
  }

  /* A with statement is a way to simplify repeated references to the same
     object, where the handle is either a class or struct instance.  */

  void visit (WithStatement *s) final override
  {
    this->start_scope (level_with);

    if (s->wthis)
      {
	/* Perform initialisation of the `with' handle.  */
	ExpInitializer *ie = s->wthis->_init->isExpInitializer ();
	gcc_assert (ie != NULL);

	declare_local_var (s->wthis);
	tree init = build_expr_dtor (ie->exp);
	add_stmt (init);
      }

    if (s->_body)
      this->build_stmt (s->_body);

    this->finish_scope ();
  }

  /* Implements `throw Object'.  Frontend already checks that the object
     thrown is a class type, but does not check if it is derived from
     Object.  Foreign objects are not currently supported at run-time.  */

  void visit (ThrowStatement *s) final override
  {
    ClassDeclaration *cd = s->exp->type->toBasetype ()->isClassHandle ();
    InterfaceDeclaration *id = cd->isInterfaceDeclaration ();
    tree arg = build_expr_dtor (s->exp);

    if (!global.params.useExceptions)
      {
	static int warned = 0;
	if (!warned)
	  {
	    error_at (make_location_t (s->loc), "exception handling disabled; "
		      "use %<-fexceptions%> to enable");
	    warned = 1;
	  }
      }

    if (cd->isCPPclass () || (id != NULL && id->isCPPclass ()))
      error_at (make_location_t (s->loc), "cannot throw C++ classes");
    else if (cd->com || (id != NULL && id->com))
      error_at (make_location_t (s->loc), "cannot throw COM objects");
    else
      arg = build_nop (build_ctype (get_object_type ()), arg);

    add_stmt (build_libcall (LIBCALL_THROW, Type::tvoid, 1, arg));
  }

  /* Build a try-catch statement, one of the building blocks for exception
     handling generated by the frontend.  This is also used to implement
     `scope (failure)' statements.  */

  void visit (TryCatchStatement *s) final override
  {
    this->start_scope (level_try);
    if (s->_body)
      this->build_stmt (s->_body);

    tree trybody = this->end_scope ();

    /* Try handlers go in their own statement list.  */
    push_stmt_list ();

    if (s->catches)
      {
	for (size_t i = 0; i < s->catches->length; i++)
	  {
	    Catch *vcatch = (*s->catches)[i];

	    this->start_scope (level_catch);

	    tree ehptr = builtin_decl_explicit (BUILT_IN_EH_POINTER);
	    tree catchtype = build_ctype (vcatch->type);
	    tree object = NULL_TREE;

	    ehptr = build_call_expr (ehptr, 1, integer_zero_node);

	    /* Retrieve the internal exception object, which could be for a
	       D or C++ catch handler.  This is different from the generic
	       exception pointer returned from gcc runtime.  */
	    Type *tcatch = vcatch->type->toBasetype ();
	    ClassDeclaration *cd = tcatch->isClassHandle ();

	    libcall_fn libcall = (cd->isCPPclass ()) ? LIBCALL_CXA_BEGIN_CATCH
	      : LIBCALL_BEGIN_CATCH;
	    object = build_libcall (libcall, vcatch->type, 1, ehptr);

	    if (vcatch->var)
	      {
		tree var = get_symbol_decl (vcatch->var);
		tree init = build_assign (INIT_EXPR, var, object);

		declare_local_var (vcatch->var);
		add_stmt (init);
	      }
	    else
	      {
		/* Still need to emit a call to __gdc_begin_catch() to
		   remove the object from the uncaught exceptions list.  */
		add_stmt (object);
	      }

	    if (vcatch->handler)
	      this->build_stmt (vcatch->handler);

	    tree catchbody = this->end_scope ();

	    /* Need to wrap C++ handlers in a try/finally block to signal
	       the end catch callback.  */
	    if (cd->isCPPclass ())
	      {
		tree endcatch = build_libcall (LIBCALL_CXA_END_CATCH,
					       Type::tvoid, 0);
		catchbody = build2 (TRY_FINALLY_EXPR, void_type_node,
				    catchbody, endcatch);
	      }

	    add_stmt (build2 (CATCH_EXPR, void_type_node,
			      catchtype, catchbody));
	  }
      }

    tree catches = pop_stmt_list ();

    /* Back-end expects all catches in a TRY_CATCH_EXPR to be enclosed in a
       statement list, however pop_stmt_list may optimize away the list
       if there is only a single catch to push.  */
    if (TREE_CODE (catches) != STATEMENT_LIST)
      {
	tree stmt_list = alloc_stmt_list ();
	append_to_statement_list_force (catches, &stmt_list);
	catches = stmt_list;
      }

    add_stmt (build2 (TRY_CATCH_EXPR, void_type_node, trybody, catches));
  }

  /* Build a try-finally statement, one of the building blocks for exception
     handling generated by the frontend.  This is also used to implement
     `scope (exit)' statements.  */

  void visit (TryFinallyStatement *s) final override
  {
    this->start_scope (level_try);
    if (s->_body)
      this->build_stmt (s->_body);

    tree trybody = this->end_scope ();

    this->start_scope (level_finally);
    if (s->finalbody)
      this->build_stmt (s->finalbody);

    tree finally = this->end_scope ();

    add_stmt (build2 (TRY_FINALLY_EXPR, void_type_node, trybody, finally));
  }

  /* The frontend lowers `synchronized (...)' statements as a call to
     monitor/critical enter and exit wrapped around try/finally.
     This visitor is not strictly required other than to enforce that
     these kinds of statements never reach here.  */

  void visit (SynchronizedStatement *) final override
  {
    gcc_unreachable ();
  }

  /* D Inline Assembler is not implemented, as it would require writing
     an assembly parser for each supported target.  Instead we leverage
     GCC extended assembler using the GccAsmStatement class.  */

  void visit (AsmStatement *) final override
  {
    sorry ("D inline assembler statements are not supported in GDC.");
  }

  /* Build a GCC extended assembler expression, whose components are
     an INSN string, some OUTPUTS, some INPUTS, and some CLOBBERS.  */

  void visit (GccAsmStatement *s) final override
  {
    StringExp *insn = s->insn->toStringExp ();
    tree outputs = NULL_TREE;
    tree inputs = NULL_TREE;
    tree clobbers = NULL_TREE;
    tree labels = NULL_TREE;

    /* Collect all arguments, which may be input or output operands.  */
    if (s->args)
      {
	for (size_t i = 0; i < s->args->length; i++)
	  {
	    Identifier *name = (*s->names)[i];
	    const char *sname = name ? name->toChars () : NULL;
	    tree id = name ? build_string (strlen (sname), sname) : NULL_TREE;

	    StringExp *constr = (*s->constraints)[i]->toStringExp ();
	    const char *cstring = (const char *)(constr->len
						 ? constr->string : "");
	    tree str = build_string (constr->len, cstring);

	    Expression *earg = (*s->args)[i];
	    tree val = build_expr (earg);

	    if (i < s->outputargs)
	      {
		tree arg = build_tree_list (id, str);
		outputs = chainon (outputs, build_tree_list (arg, val));
	      }
	    else
	      {
		tree arg = build_tree_list (id, str);
		inputs = chainon (inputs, build_tree_list (arg, val));
	      }
	  }
      }

    /* Collect all clobber arguments.  */
    if (s->clobbers)
      {
	for (size_t i = 0; i < s->clobbers->length; i++)
	  {
	    StringExp *clobber = (*s->clobbers)[i]->toStringExp ();
	    const char *cstring = (const char *)(clobber->len
						 ? clobber->string : "");

	    tree val = build_string (clobber->len, cstring);
	    clobbers = chainon (clobbers, build_tree_list (0, val));
	  }
      }

    /* Collect all goto labels, these should have been already checked
       by the front-end, so pass down the label symbol to the back-end.  */
    if (s->labels)
      {
	for (size_t i = 0; i < s->labels->length; i++)
	  {
	    Identifier *ident = (*s->labels)[i];
	    GotoStatement *gs = (*s->gotos)[i];

	    gcc_assert (gs->label->statement != NULL);
	    gcc_assert (gs->tf == gs->label->statement->tf);

	    const char *sident = ident->toChars ();
	    tree name = build_string (strlen (sident), sident);
	    tree label = this->lookup_label (gs->label->statement,
					     gs->label->ident);
	    TREE_USED (label) = 1;

	    labels = chainon (labels, build_tree_list (name, label));
	  }
      }

    /* Do some extra validation on all input and output operands.  */
    const char *insnstring = (const char *)(insn->len ? insn->string : "");
    tree string = build_string (insn->len, insnstring);
    string = resolve_asm_operand_names (string, outputs, inputs, labels);

    if (s->args)
      {
	unsigned noutputs = s->outputargs;
	unsigned ninputs = (s->args->length - noutputs);
	const char **oconstraints = XALLOCAVEC (const char *, noutputs);
	bool allows_mem, allows_reg, is_inout;
	size_t i;
	tree t;

	for (i = 0, t = outputs; t != NULL_TREE; t = TREE_CHAIN (t), i++)
	  {
	    tree output = TREE_VALUE (t);
	    const char *constraint
	      = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));

	    oconstraints[i] = constraint;

	    if (parse_output_constraint (&constraint, i, ninputs, noutputs,
					 &allows_mem, &allows_reg, &is_inout))
	      {
		/* If the output argument is going to end up in memory.  */
		if (!allows_reg)
		  d_mark_addressable (output);
	      }
	    else
	      output = error_mark_node;

	    TREE_VALUE (t) = output;
	  }

	for (i = 0, t = inputs; t != NULL_TREE; t = TREE_CHAIN (t), i++)
	  {
	    tree input = TREE_VALUE (t);
	    const char *constraint
	      = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));

	    if (parse_input_constraint (&constraint, i, ninputs, noutputs, 0,
					oconstraints, &allows_mem, &allows_reg))
	      {
		/* If the input argument is going to end up in memory.  */
		if (!allows_reg && allows_mem)
		  d_mark_addressable (input);
	      }
	    else
	      input = error_mark_node;

	    TREE_VALUE (t) = input;
	  }
      }

    tree exp = build5 (ASM_EXPR, void_type_node, string,
		       outputs, inputs, clobbers, labels);
    SET_EXPR_LOCATION (exp, make_location_t (s->loc));

    /* If the extended syntax was not used, mark the ASM_EXPR as being an
       ASM_INPUT expression instead of an ASM_OPERAND with no operands.  */
    if (s->args == NULL && s->clobbers == NULL)
      ASM_INPUT_P (exp) = 1;

    /* All asm statements are assumed to have a side effect.  As a future
       optimization, this could be unset when building in release mode.  */
    ASM_VOLATILE_P (exp) = 1;

    /* If the function has been annotated with `pragma(inline)', then mark
       the asm expression as being inline as well.  */
    if (this->func_->inlining == PINLINE::always)
      ASM_INLINE_P (exp) = 1;

    add_stmt (exp);
  }

  /* Import symbols from another module.  */

  void visit (ImportStatement *s) final override
  {
    if (s->imports == NULL)
      return;

    for (size_t i = 0; i < s->imports->length; i++)
      {
	Dsymbol *dsym = (*s->imports)[i];

	if (dsym != NULL)
	  build_decl_tree (dsym);
      }
  }
};

/* Main entry point for the IRVisitor interface to generate
   code for the body of function FD.  */

void
build_function_body (FuncDeclaration *fd)
{
  IRVisitor v = IRVisitor (fd);
  location_t saved_location = input_location;
  input_location = make_location_t (fd->loc);
  v.build_stmt (fd->fbody);
  input_location = saved_location;
}
