/* m2block.cc provides an interface to maintaining block structures.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#define m2block_c
#include "m2assert.h"
#include "m2block.h"
#include "m2decl.h"
#include "m2options.h"
#include "m2tree.h"
#include "m2treelib.h"

/* For each binding contour we allocate a binding_level structure
   which records the entities defined or declared in that contour.
   Contours include:

   the global one one for each subprogram definition

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct GTY (()) binding_level
{
  /* The function associated with the scope.  This is NULL_TREE for the
     global scope.  */
  tree fndecl;

  /* A chain of _DECL nodes for all variables, constants, functions,
     and typedef types.  These are in the reverse of the order supplied.  */
  tree names;

  /* A boolean to indicate whether this is binding level is a global ie
     outer module scope.  In which case fndecl will be NULL_TREE.  */
  int is_global;

  /* The context of the binding level, for a function binding level
     this will be the same as fndecl, however for a global binding level
     this is a translation_unit.  */
  tree context;

  /* The binding level below this one.  This field is only used when
     the binding level has been pushed by pushFunctionScope.  */
  struct binding_level *next;

  /* All binding levels are placed onto this list.  */
  struct binding_level *list;

  /* A varray of trees, which represent the list of statement
     sequences.  */
  vec<tree, va_gc> *m2_statements;

  /* A list of constants (only kept in the global binding level).
     Constants need to be kept through the life of the compilation, as the
     same constants can be used in any scope.  */
  tree constants;

  /* A list of inner module initialization functions.  */
  tree init_functions;

  /* A list of types created by M2GCCDeclare prior to code generation
     and those which may not be specifically declared and saved via a
     push_decl.  */
  tree types;

  /* A list of all DECL_EXPR created within this binding level.  This
     will be prepended to the statement list once the binding level (scope
     is finished).  */
  tree decl;

  /* A list of labels which have been created in this scope.  */
  tree labels;

  /* The number of times this level has been pushed.  */
  int count;
};

/* The binding level currently in effect.  */

static GTY (()) struct binding_level *current_binding_level;

/* The outermost binding level, for names of file scope.  This is
   created when the compiler is started and exists through the entire
   run.  */

static GTY (()) struct binding_level *global_binding_level;

/* The head of the binding level lists.  */
static GTY (()) struct binding_level *head_binding_level;

/* The current statement tree.  */

typedef struct stmt_tree_s *stmt_tree_t;

#undef DEBUGGING

static location_t pending_location;
static int pending_statement = false;

/* assert_global_names asserts that the global_binding_level->names
   can be chained.  */

static void
assert_global_names (void)
{
  tree p = global_binding_level->names;

  while (p)
    p = TREE_CHAIN (p);
}

/* lookupLabel return label tree in current scope, otherwise
   NULL_TREE.  */

static tree
lookupLabel (tree id)
{
  tree t;

  for (t = current_binding_level->labels; t != NULL_TREE; t = TREE_CHAIN (t))
    {
      tree l = TREE_VALUE (t);

      if (id == DECL_NAME (l))
        return l;
    }
  return NULL_TREE;
}

/* getLabel return the label name or create a label name in the
   current scope.  */

tree
m2block_getLabel (location_t location, char *name)
{
  tree id = get_identifier (name);
  tree label = lookupLabel (id);

  if (label == NULL_TREE)
    {
      label = build_decl (location, LABEL_DECL, id, void_type_node);
      current_binding_level->labels
          = tree_cons (NULL_TREE, label, current_binding_level->labels);
    }
  if (DECL_CONTEXT (label) == NULL_TREE)
    DECL_CONTEXT (label) = current_function_decl;
  ASSERT ((DECL_CONTEXT (label) == current_function_decl),
          current_function_decl);

  DECL_MODE (label) = VOIDmode;
  return label;
}

static void
init_binding_level (struct binding_level *l)
{
  l->fndecl = NULL;
  l->names = NULL;
  l->is_global = 0;
  l->context = NULL;
  l->next = NULL;
  l->list = NULL;
  vec_alloc (l->m2_statements, 1);
  l->constants = NULL;
  l->init_functions = NULL;
  l->types = NULL;
  l->decl = NULL;
  l->labels = NULL;
  l->count = 0;
}

static struct binding_level *
newLevel (void)
{
  struct binding_level *newlevel = ggc_alloc<binding_level> ();

  init_binding_level (newlevel);

  /* Now we a push_statement_list.  */
  vec_safe_push (newlevel->m2_statements, m2block_begin_statement_list ());
  return newlevel;
}

tree *
m2block_cur_stmt_list_addr (void)
{
  ASSERT_CONDITION (current_binding_level != NULL);
  int l = vec_safe_length (current_binding_level->m2_statements) - 1;

  return &(*current_binding_level->m2_statements)[l];
}

tree
m2block_cur_stmt_list (void)
{
  tree *t = m2block_cur_stmt_list_addr ();

  return *t;
}

/* is_building_stmt_list returns true if we are building a
   statement list.  true is returned if we are in a binding level and
   a statement list is under construction.  */

int
m2block_is_building_stmt_list (void)
{
  ASSERT_CONDITION (current_binding_level != NULL);
  return !vec_safe_is_empty (current_binding_level->m2_statements);
}

/* push_statement_list pushes the statement list t onto the
   current binding level.  */

tree
m2block_push_statement_list (tree t)
{
  ASSERT_CONDITION (current_binding_level != NULL);
  vec_safe_push (current_binding_level->m2_statements, t);
  return t;
}

/* pop_statement_list pops and returns a statement list from the
   current binding level.  */

tree
m2block_pop_statement_list (void)
{
  ASSERT_CONDITION (current_binding_level != NULL);
  {
    tree t = current_binding_level->m2_statements->pop ();

    return t;
  }
}

/* begin_statement_list starts a tree statement.  It pushes the
   statement list and returns the list node.  */

tree
m2block_begin_statement_list (void)
{
  return alloc_stmt_list ();
}

/* findLevel returns the binding level associated with fndecl one
   is created if there is no existing one on head_binding_level.  */

static struct binding_level *
findLevel (tree fndecl)
{
  struct binding_level *b;

  if (fndecl == NULL_TREE)
    return global_binding_level;

  b = head_binding_level;
  while ((b != NULL) && (b->fndecl != fndecl))
    b = b->list;

  if (b == NULL)
    {
      b = newLevel ();
      b->fndecl = fndecl;
      b->context = fndecl;
      b->is_global = false;
      b->list = head_binding_level;
      b->next = NULL;
    }
  return b;
}

/* pushFunctionScope push a binding level.  */

void
m2block_pushFunctionScope (tree fndecl)
{
  struct binding_level *n;
  struct binding_level *b;

#if defined(DEBUGGING)
  if (fndecl != NULL)
    printf ("pushFunctionScope\n");
#endif

  /* Allow multiple consecutive pushes of the same scope.  */

  if (current_binding_level != NULL
      && (current_binding_level->fndecl == fndecl))
    {
      current_binding_level->count++;
      return;
    }

  /* Firstly check to see that fndecl is not already on the binding
     stack.  */

  for (b = current_binding_level; b != NULL; b = b->next)
    /* Only allowed one instance of the binding on the stack at a time.  */
    ASSERT_CONDITION (b->fndecl != fndecl);

  n = findLevel (fndecl);

  /* Add this level to the front of the stack.  */
  n->next = current_binding_level;
  current_binding_level = n;
}

/* popFunctionScope - pops a binding level, returning the function
   associated with the binding level.  */

tree
m2block_popFunctionScope (void)
{
  tree fndecl = current_binding_level->fndecl;

#if defined(DEBUGGING)
  if (fndecl != NULL)
    printf ("popFunctionScope\n");
#endif

  if (current_binding_level->count > 0)
    {
      /* Multiple pushes have occurred of the same function scope (and
         ignored), pop them likewise.  */
      current_binding_level->count--;
      return fndecl;
    }
  ASSERT_CONDITION (current_binding_level->fndecl
                    != NULL_TREE); /* Expecting local scope.  */

  ASSERT_CONDITION (current_binding_level->constants
                    == NULL_TREE); /* Should not be used.  */
  ASSERT_CONDITION (current_binding_level->names
                    == NULL_TREE); /* Should be cleared.  */
  ASSERT_CONDITION (current_binding_level->decl
                    == NULL_TREE); /* Should be cleared.  */

  current_binding_level = current_binding_level->next;
  return fndecl;
}

/* pushGlobalScope push the global scope onto the binding level
   stack.  There can only ever be one instance of the global binding
   level on the stack.  */

void
m2block_pushGlobalScope (void)
{
#if defined(DEBUGGING)
  printf ("pushGlobalScope\n");
#endif
  m2block_pushFunctionScope (NULL_TREE);
}

/* popGlobalScope pops the current binding level, it expects this
   binding level to be the global binding level.  */

void
m2block_popGlobalScope (void)
{
  ASSERT_CONDITION (
      current_binding_level->is_global);  /* Expecting global scope.  */
  ASSERT_CONDITION (current_binding_level == global_binding_level);

  if (current_binding_level->count > 0)
    {
      current_binding_level->count--;
      return;
    }

  current_binding_level = current_binding_level->next;
#if defined(DEBUGGING)
  printf ("popGlobalScope\n");
#endif

  assert_global_names ();
}

/* finishFunctionDecl removes declarations from the current binding
   level and places them inside fndecl.  The current binding level is
   then able to be destroyed by a call to popFunctionScope.

   The extra tree nodes associated with fndecl will be created such
   as BIND_EXPR, BLOCK and the initial STATEMENT_LIST containing the
   DECL_EXPR is also created.  */

void
m2block_finishFunctionDecl (location_t location, tree fndecl)
{
  tree context = current_binding_level->context;
  tree block = DECL_INITIAL (fndecl);
  tree bind_expr = DECL_SAVED_TREE (fndecl);
  tree i;

  if (block == NULL_TREE)
    {
      block = make_node (BLOCK);
      DECL_INITIAL (fndecl) = block;
      TREE_USED (block) = true;
      BLOCK_SUBBLOCKS (block) = NULL_TREE;
    }
  BLOCK_SUPERCONTEXT (block) = context;

  BLOCK_VARS (block)
      = chainon (BLOCK_VARS (block), current_binding_level->names);
  TREE_USED (fndecl) = true;

  if (bind_expr == NULL_TREE)
    {
      bind_expr
          = build3 (BIND_EXPR, void_type_node, current_binding_level->names,
                    current_binding_level->decl, block);
      DECL_SAVED_TREE (fndecl) = bind_expr;
    }
  else
    {
      if (!chain_member (current_binding_level->names,
                         BIND_EXPR_VARS (bind_expr)))
        {
          BIND_EXPR_VARS (bind_expr) = chainon (BIND_EXPR_VARS (bind_expr),
                                                current_binding_level->names);

          if (current_binding_level->names != NULL_TREE)
            {
              for (i = current_binding_level->names; i != NULL_TREE;
                   i = DECL_CHAIN (i))
                append_to_statement_list_force (i,
                                                &BIND_EXPR_BODY (bind_expr));

            }
        }
    }
  SET_EXPR_LOCATION (bind_expr, location);

  current_binding_level->names = NULL_TREE;
  current_binding_level->decl = NULL_TREE;
}

/* finishFunctionCode adds cur_stmt_list to fndecl.  The current
   binding level is then able to be destroyed by a call to
   popFunctionScope.  The cur_stmt_list is appended to the
   STATEMENT_LIST.  */

void
m2block_finishFunctionCode (tree fndecl)
{
  tree bind_expr;
  tree block;
  tree statements = m2block_pop_statement_list ();
  tree_stmt_iterator i;

  ASSERT_CONDITION (DECL_SAVED_TREE (fndecl) != NULL_TREE);

  bind_expr = DECL_SAVED_TREE (fndecl);
  ASSERT_CONDITION (TREE_CODE (bind_expr) == BIND_EXPR);

  block = DECL_INITIAL (fndecl);
  ASSERT_CONDITION (TREE_CODE (block) == BLOCK);

  if (current_binding_level->names != NULL_TREE)
    {
      BIND_EXPR_VARS (bind_expr)
          = chainon (BIND_EXPR_VARS (bind_expr), current_binding_level->names);
      current_binding_level->names = NULL_TREE;
    }
  if (current_binding_level->labels != NULL_TREE)
    {
      tree t;

      for (t = current_binding_level->labels; t != NULL_TREE;
           t = TREE_CHAIN (t))
        {
          tree l = TREE_VALUE (t);

          BIND_EXPR_VARS (bind_expr) = chainon (BIND_EXPR_VARS (bind_expr), l);
        }
      current_binding_level->labels = NULL_TREE;
    }

  BLOCK_VARS (block) = BIND_EXPR_VARS (bind_expr);

  if (current_binding_level->decl != NULL_TREE)
    for (i = tsi_start (current_binding_level->decl); !tsi_end_p (i);
         tsi_next (&i))
      append_to_statement_list_force (*tsi_stmt_ptr (i),
                                      &BIND_EXPR_BODY (bind_expr));

  for (i = tsi_start (statements); !tsi_end_p (i); tsi_next (&i))
    append_to_statement_list_force (*tsi_stmt_ptr (i),
                                    &BIND_EXPR_BODY (bind_expr));

  current_binding_level->decl = NULL_TREE;
}

void
m2block_finishGlobals (void)
{
  tree context = global_binding_level->context;
  tree block = make_node (BLOCK);
  tree p = global_binding_level->names;

  BLOCK_SUBBLOCKS (block) = NULL;
  TREE_USED (block) = 1;

  BLOCK_VARS (block) = p;

  DECL_INITIAL (context) = block;
  BLOCK_SUPERCONTEXT (block) = context;
}

/* pushDecl pushes a declaration onto the current binding level.  */

tree
m2block_pushDecl (tree decl)
{
  /* External objects aren't nested, other objects may be.  */

  if (decl != current_function_decl)
    DECL_CONTEXT (decl) = current_binding_level->context;

  /* Put the declaration on the list.  The list of declarations is in
     reverse order.  The list will be reversed later if necessary.  This
     needs to be this way for compatibility with the back-end.  */

  TREE_CHAIN (decl) = current_binding_level->names;
  current_binding_level->names = decl;

  assert_global_names ();

  return decl;
}

/* includeDecl pushes a declaration onto the current binding level
   providing it is not already present.  */

void
m2block_includeDecl (tree decl)
{
  tree p = current_binding_level->names;

  while (p != decl && p != NULL)
    p = TREE_CHAIN (p);
  if (p != decl)
    m2block_pushDecl (decl);
}

/* addDeclExpr adds the DECL_EXPR node t to the statement list
   current_binding_level->decl.  This allows us to order all
   declarations at the beginning of the function.  */

void
m2block_addDeclExpr (tree t)
{
  append_to_statement_list_force (t, &current_binding_level->decl);
}

/* RememberType remember the type t in the ggc marked list.  */

tree
m2block_RememberType (tree t)
{
  global_binding_level->types
      = tree_cons (NULL_TREE, t, global_binding_level->types);
  return t;
}

/* global_constant returns t.  It chains t onto the
   global_binding_level list of constants, if it is not already
   present.  */

tree
m2block_global_constant (tree t)
{
  tree s;

  if (global_binding_level->constants != NULL_TREE)
    for (s = global_binding_level->constants; s != NULL_TREE;
         s = TREE_CHAIN (s))
      {
        tree c = TREE_VALUE (s);

        if (c == t)
          return t;
      }

  global_binding_level->constants
      = tree_cons (NULL_TREE, t, global_binding_level->constants);
  return t;
}

/* RememberConstant adds a tree t onto the list of constants to
   be marked whenever the ggc re-marks all used storage.  Constants
   live throughout the whole compilation and they can be used by
   many different functions if necessary.  */

tree
m2block_RememberConstant (tree t)
{
  if ((t != NULL) && (m2tree_IsAConstant (t)))
    return m2block_global_constant (t);
  return t;
}

/* DumpGlobalConstants displays all global constants and checks
   none are poisoned.  */

tree
m2block_DumpGlobalConstants (void)
{
  tree s;

  if (global_binding_level->constants != NULL_TREE)
    for (s = global_binding_level->constants; TREE_CHAIN (s);
         s = TREE_CHAIN (s))
      debug_tree (s);
  return NULL_TREE;
}

/* RememberInitModuleFunction records tree t in the global
   binding level.  So that it will not be garbage collected.  In
   theory the inner modules could be placed inside the
   current_binding_level I suspect.  */

tree
m2block_RememberInitModuleFunction (tree t)
{
  global_binding_level->init_functions
      = tree_cons (NULL_TREE, t, global_binding_level->init_functions);
  return t;
}

/* toplevel return true if we are in the global scope.  */

bool
m2block_toplevel (void)
{
  if (current_binding_level == NULL)
    return true;
  if (current_binding_level->fndecl == NULL)
    return true;
  return false;
}

/* GetErrorNode returns the gcc error_mark_node.  */

tree
m2block_GetErrorNode (void)
{
  return error_mark_node;
}

/* GetGlobals - returns a list of global variables, functions,
   constants.  */

tree
m2block_GetGlobals (void)
{
  assert_global_names ();
  return global_binding_level->names;
}

/* GetGlobalContext - returns the global context tree.  */

tree
m2block_GetGlobalContext (void)
{
  return global_binding_level->context;
}

/* do_add_stmt - t is a statement.  Add it to the statement-tree.  */

static tree
do_add_stmt (tree t)
{
  if (current_binding_level != NULL)
    append_to_statement_list_force (t, m2block_cur_stmt_list_addr ());
  return t;
}

/* flush_pending_note - flushes a pending_statement note if
   necessary.  */

static void
flush_pending_note (void)
{
  if (pending_statement && (M2Options_GetM2g ()))
    {
#if 0
      /* --fixme-- we need a machine independant way to generate a nop.  */
      tree instr = m2decl_BuildStringConstant ("nop", 3);
      tree string
          = resolve_asm_operand_names (instr, NULL_TREE, NULL_TREE, NULL_TREE);
      tree note = build_stmt (pending_location, ASM_EXPR, string, NULL_TREE,
                              NULL_TREE, NULL_TREE, NULL_TREE);

      ASM_BASIC_P (note) = false;
      ASM_VOLATILE_P (note) = false;
#else
      tree note = build_empty_stmt (pending_location);
#endif
      pending_statement = false;
      do_add_stmt (note);
    }
}

/* add_stmt t is a statement.  Add it to the statement-tree.  */

tree
m2block_add_stmt (location_t location, tree t)
{
  if ((CAN_HAVE_LOCATION_P (t)) && (!EXPR_HAS_LOCATION (t)))
    SET_EXPR_LOCATION (t, location);

  if (pending_statement && (pending_location != location))
    flush_pending_note ();

  pending_statement = false;
  return do_add_stmt (t);
}

/* addStmtNote remember this location represents the start of a
   Modula-2 statement.  It is flushed if another different location
   is generated or another tree is given to add_stmt.  */

void
m2block_addStmtNote (location_t location)
{
  if (pending_statement && (pending_location != location))
    flush_pending_note ();

  pending_statement = true;
  pending_location = location;
}

void
m2block_removeStmtNote (void)
{
  pending_statement = false;
}

/* init - initialize the data structures in this module.  */

void
m2block_init (void)
{
  global_binding_level = newLevel ();
  global_binding_level->context = build_translation_unit_decl (NULL);
  global_binding_level->is_global = true;
  current_binding_level = NULL;
}

#include "gt-m2-m2block.h"
