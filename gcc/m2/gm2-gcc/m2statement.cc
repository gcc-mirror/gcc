/* m2statement.cc provides an interface to GCC statement trees.

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

#include "../gm2-lang.h"
#include "../m2-tree.h"

/* Prototypes.  */

#define m2statement_c
#include "m2assert.h"
#include "m2block.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2statement.h"
#include "m2tree.h"
#include "m2treelib.h"
#include "m2type.h"
#include "m2convert.h"
#include "m2pp.h"

static GTY (()) tree param_list = NULL_TREE; /* Ready for the next time we
                                                call/define a function.  */
static GTY (()) tree last_function = NULL_TREE;


/* BuildStartFunctionCode - generate function entry code.  */

void
m2statement_BuildStartFunctionCode (location_t location, tree fndecl,
                                    bool isexported, bool isinline)
{
  tree param_decl;

  ASSERT_BOOL (isexported);
  ASSERT_BOOL (isinline);
  /* Announce we are compiling this function.  */
  announce_function (fndecl);

  /* Set up to compile the function and enter it.  */

  DECL_INITIAL (fndecl) = NULL_TREE;

  current_function_decl = fndecl;
  m2block_pushFunctionScope (fndecl);
  m2statement_SetBeginLocation (location);

  ASSERT_BOOL ((cfun != NULL));
  /* Initialize the RTL code for the function.  */
  allocate_struct_function (fndecl, false);
  /* Begin the statement tree for this function.  */
  DECL_SAVED_TREE (fndecl) = NULL_TREE;

  /* Set the context of these parameters to this function.  */
  for (param_decl = DECL_ARGUMENTS (fndecl); param_decl;
       param_decl = TREE_CHAIN (param_decl))
    DECL_CONTEXT (param_decl) = fndecl;

  /* This function exists in static storage.  (This does not mean
  `static' in the C sense!) */
  TREE_STATIC (fndecl) = 1;
  TREE_PUBLIC (fndecl) = isexported;
  /* We could do better here by detecting ADR
     or type PROC used on this function.  --fixme--  */
  TREE_ADDRESSABLE (fndecl) = 1;
  DECL_DECLARED_INLINE_P (fndecl) = 0; /* isinline;  */
}

/* BuildEndFunctionCode - generates the function epilogue.  */

void
m2statement_BuildEndFunctionCode (location_t location, tree fndecl, bool nested)
{
  tree block = DECL_INITIAL (fndecl);

  BLOCK_SUPERCONTEXT (block) = fndecl;

  /* Must mark the RESULT_DECL as being in this function.  */
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  /* And attach it to the function.  */
  DECL_INITIAL (fndecl) = block;

  m2block_finishFunctionCode (fndecl);
  m2statement_SetEndLocation (location);

  m2pp_dump_gimple (M2PP_DUMP_PRE_GENERICIZE, fndecl);
  gm2_genericize (fndecl);
  if (nested)
    (void)cgraph_node::get_create (fndecl);
  else
    {
      m2pp_dump_gimple (M2PP_DUMP_POST_GENERICIZE, fndecl);
      cgraph_node::finalize_function (fndecl, false);
    }

  m2block_popFunctionScope ();

  /* We're leaving the context of this function, so zap cfun.  It's
     still in DECL_STRUCT_FUNCTION, and we'll restore it in
     tree_rest_of_compilation.  */
  set_cfun (NULL);
  current_function_decl = NULL;
}

/* BuildPushFunctionContext - pushes the current function context.
   Maps onto push_function_context in ../function.cc.  */

void
m2statement_BuildPushFunctionContext (void)
{
  push_function_context ();
}

/* BuildPopFunctionContext - pops the current function context.  Maps
   onto pop_function_context in ../function.cc.  */

void
m2statement_BuildPopFunctionContext (void)
{
  pop_function_context ();
}

void
m2statement_SetBeginLocation (location_t location)
{
  if (cfun != NULL)
    cfun->function_start_locus = location;
}

void
m2statement_SetEndLocation (location_t location)
{
  if (cfun != NULL)
    cfun->function_end_locus = location;
}

/* BuildAssignmentTree builds the assignment of, des, and, expr.
   It returns, des.  */

tree
m2statement_BuildAssignmentTree (location_t location, tree des, tree expr)
{
  tree result;

  m2assert_AssertLocation (location);
  STRIP_TYPE_NOPS (expr);

  if (TREE_CODE (expr) == FUNCTION_DECL)
    result = build2 (MODIFY_EXPR, TREE_TYPE (des), des,
                     m2expr_BuildAddr (location, expr, false));
  else
    {
      gcc_assert (TREE_CODE (TREE_TYPE (des)) != TYPE_DECL);
      if (TREE_TYPE (expr) == TREE_TYPE (des))
        result = build2 (MODIFY_EXPR, TREE_TYPE (des), des, expr);
      else
        result = build2 (
            MODIFY_EXPR, TREE_TYPE (des), des,
            m2convert_BuildConvert (location, TREE_TYPE (des), expr, false));
    }

  TREE_SIDE_EFFECTS (result) = true;
  TREE_USED (des) = true;
  TREE_USED (expr) = true;
  add_stmt (location, result);
  return des;
}

/* BuildAssignmentStatement builds the assignment of, des, and, expr.  */

void
m2statement_BuildAssignmentStatement (location_t location, tree des, tree expr)
{
  m2statement_BuildAssignmentTree (location, des, expr);
}

/* BuildGoto builds a goto operation.  */

void
m2statement_BuildGoto (location_t location, char *name)
{
  tree label = m2block_getLabel (location, name);

  m2assert_AssertLocation (location);
  TREE_USED (label) = true;
  add_stmt (location, build1 (GOTO_EXPR, void_type_node, label));
}

/* DeclareLabel - create a label, name.  */

void
m2statement_DeclareLabel (location_t location, char *name)
{
  tree label = m2block_getLabel (location, name);

  m2assert_AssertLocation (location);
  add_stmt (location, build1 (LABEL_EXPR, void_type_node, label));
}

/* BuildParam - build a list of parameters, ready for a subsequent
   procedure call.  */

void
m2statement_BuildParam (location_t location, tree param)
{
  m2assert_AssertLocation (location);

  TREE_USED (param) = true;
  if (TREE_CODE (param) == FUNCTION_DECL)
    param = m2expr_BuildAddr (location, param, false);

  param_list = chainon (build_tree_list (NULL_TREE, param), param_list);
}

/* nCount - return the number of chained tree nodes in list, t.  */

static int
nCount (tree t)
{
  int i = 0;

  while (t != NULL)
    {
      i++;
      t = TREE_CHAIN (t);
    }
  return i;
}

/* BuildProcedureCallTree - creates a procedure call from a procedure
   and parameter list and the return type, rettype.  */

tree
m2statement_BuildProcedureCallTree (location_t location, tree procedure,
                                    tree rettype)
{
  tree functype = TREE_TYPE (procedure);
  tree funcptr = build1 (ADDR_EXPR, build_pointer_type (functype), procedure);
  tree call;
  int n = nCount (param_list);
  tree *argarray = XALLOCAVEC (tree, n);
  tree t = param_list;
  int i;

  m2assert_AssertLocation (location);
  ASSERT_CONDITION (
      last_function
      == NULL_TREE); /* Previous function value has not been collected.  */
  TREE_USED (procedure) = true;

  for (i = 0; i < n; i++)
    {
      argarray[i] = TREE_VALUE (t);
      t = TREE_CHAIN (t);
    }

  if (rettype == NULL_TREE)
    {
      rettype = void_type_node;
      call = build_call_array_loc (location, rettype, funcptr, n, argarray);
      TREE_USED (call) = true;
      TREE_SIDE_EFFECTS (call) = true;

#if defined(DEBUG_PROCEDURE_CALLS)
      fprintf (stderr, "built the modula-2 call, here is the tree\n");
      fflush (stderr);
      debug_tree (call);
#endif

      param_list
          = NULL_TREE; /* Ready for the next time we call a procedure.  */
      last_function = NULL_TREE;
      return call;
    }
  else
    {
      last_function = build_call_array_loc (
          location, m2tree_skip_type_decl (rettype), funcptr, n, argarray);
      TREE_USED (last_function) = true;
      TREE_SIDE_EFFECTS (last_function) = true;
      param_list
          = NULL_TREE; /* Ready for the next time we call a procedure.  */
      return last_function;
    }
}

/* BuildIndirectProcedureCallTree - creates a procedure call from a
   procedure and parameter list and the return type, rettype.  */

tree
m2statement_BuildIndirectProcedureCallTree (location_t location,
                                            tree procedure, tree rettype)
{
  tree call;
  int n = nCount (param_list);
  tree *argarray = XALLOCAVEC (tree, n);
  tree t = param_list;
  int i;

  m2assert_AssertLocation (location);
  TREE_USED (procedure) = true;
  TREE_SIDE_EFFECTS (procedure) = true;

  for (i = 0; i < n; i++)
    {
      argarray[i] = TREE_VALUE (t);
      t = TREE_CHAIN (t);
    }

  if (rettype == NULL_TREE)
    {
      rettype = void_type_node;
      call = build_call_array_loc (location, rettype, procedure, n, argarray);
      TREE_USED (call) = true;
      TREE_SIDE_EFFECTS (call) = true;

#if defined(DEBUG_PROCEDURE_CALLS)
      fprintf (stderr, "built the modula-2 call, here is the tree\n");
      fflush (stderr);
      debug_tree (call);
#endif

      last_function = NULL_TREE;
      param_list
          = NULL_TREE; /* Ready for the next time we call a procedure.  */
      return call;
    }
  else
    {
      last_function = build_call_array_loc (
          location, m2tree_skip_type_decl (rettype), procedure, n, argarray);
      TREE_USED (last_function) = true;
      TREE_SIDE_EFFECTS (last_function) = true;
      param_list
          = NULL_TREE; /* Ready for the next time we call a procedure.  */
      return last_function;
    }
}


/* BuildBuiltinCallTree calls the builtin procedure.  */

tree
m2statement_BuildBuiltinCallTree (tree func)
{
  TREE_USED (func) = true;
  TREE_SIDE_EFFECTS (func) = true;
  param_list
    = NULL_TREE; /* Ready for the next time we call a procedure.  */
  return func;
}


/* BuildFunctValue - generates code for value :=
   last_function(foobar); */

tree
m2statement_BuildFunctValue (location_t location, tree value)
{
  tree assign
      = m2treelib_build_modify_expr (location, value, NOP_EXPR, last_function);

  m2assert_AssertLocation (location);
  ASSERT_CONDITION (
      last_function
      != NULL_TREE);  /* No value available, possible used before.  */

  TREE_SIDE_EFFECTS (assign) = true;
  TREE_USED (assign) = true;
  TREE_USED (value) = true;
  last_function = NULL_TREE;
  return assign;
  // return m2statement_BuildAssignmentTree (location, value, assign);
}

/* BuildCall2 - builds a tree representing: function (arg1, arg2).  */

tree
m2statement_BuildCall2 (location_t location, tree function, tree rettype,
                        tree arg1, tree arg2)
{
  m2assert_AssertLocation (location);
  ASSERT_CONDITION (param_list == NULL_TREE);

  param_list = chainon (build_tree_list (NULL_TREE, arg2), param_list);
  param_list = chainon (build_tree_list (NULL_TREE, arg1), param_list);
  return m2statement_BuildProcedureCallTree (location, function, rettype);
}

/* BuildCall3 - builds a tree representing: function (arg1, arg2,
   arg3).  */

tree
m2statement_BuildCall3 (location_t location, tree function, tree rettype,
                        tree arg1, tree arg2, tree arg3)
{
  m2assert_AssertLocation (location);
  ASSERT_CONDITION (param_list == NULL_TREE);

  param_list = chainon (build_tree_list (NULL_TREE, arg3), param_list);
  param_list = chainon (build_tree_list (NULL_TREE, arg2), param_list);
  param_list = chainon (build_tree_list (NULL_TREE, arg1), param_list);
  return m2statement_BuildProcedureCallTree (location, function, rettype);
}

/* BuildFunctionCallTree - creates a procedure function call from
   a procedure and parameter list and the return type, rettype.
   No tree is returned as the tree is held in the last_function global
   variable.  It is expected the BuildFunctValue is to be called after
   a call to BuildFunctionCallTree.  */

void
m2statement_BuildFunctionCallTree (location_t location, tree procedure,
                                   tree rettype)
{
  m2statement_BuildProcedureCallTree (location, procedure, rettype);
}

/* SetLastFunction - assigns last_function to, t.  */

void
m2statement_SetLastFunction (tree t)
{
  last_function = t;
}

/* SetParamList - assigns param_list to, t.  */

void
m2statement_SetParamList (tree t)
{
  param_list = t;
}

/* GetLastFunction - returns, last_function.  */

tree
m2statement_GetLastFunction (void)
{
  return last_function;
}

/* GetParamList - returns, param_list.  */

tree
m2statement_GetParamList (void)
{
  return param_list;
}

/* GetCurrentFunction - returns the current_function.  */

tree
m2statement_GetCurrentFunction (void)
{
  return current_function_decl;
}

/* GetParamTree - return parameter, i.  */

tree
m2statement_GetParamTree (tree call, unsigned int i)
{
  return CALL_EXPR_ARG (call, i);
}

/* BuildTryFinally - returns a TRY_FINALL_EXPR with the call and
   cleanups attached.  */

tree
m2statement_BuildTryFinally (location_t location, tree call, tree cleanups)
{
  return build_stmt (location, TRY_FINALLY_EXPR, call, cleanups);
}

/* BuildCleanUp - return a CLEANUP_POINT_EXPR which will clobber,
   param.  */

tree
m2statement_BuildCleanUp (tree param)
{
  tree clobber = build_constructor (TREE_TYPE (param), NULL);
  TREE_THIS_VOLATILE (clobber) = 1;
  return build2 (MODIFY_EXPR, TREE_TYPE (param), param, clobber);
}

/* BuildAsm - generates an inline assembler instruction.  */

void
m2statement_BuildAsm (location_t location, tree instr, bool isVolatile,
                      bool isSimple, tree inputs, tree outputs, tree trash,
                      tree labels)
{
  tree string = resolve_asm_operand_names (instr, outputs, inputs, labels);
  tree args = build_stmt (location, ASM_EXPR, string, outputs, inputs, trash,
                          labels);

  m2assert_AssertLocation (location);

  /* ASM statements without outputs, including simple ones, are treated
     as volatile.  */
  ASM_BASIC_P (args) = isSimple;
  ASM_VOLATILE_P (args) = isVolatile;

  add_stmt (location, args);
}

/* BuildUnaryForeachWordDo - provides the large set operators.  Each
   word (or less) of the set can be calculated by unop.  This
   procedure runs along each word of the large set invoking the unop.  */

void
m2statement_BuildUnaryForeachWordDo (location_t location, tree type, tree op1,
                                     tree op2,
                                     tree (*unop) (location_t, tree, bool),
                                     bool is_op1lvalue, bool is_op2lvalue,
                                     bool is_op1const, bool is_op2const)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);
  ASSERT_BOOL (is_op1lvalue);
  ASSERT_BOOL (is_op2lvalue);
  ASSERT_BOOL (is_op1const);
  ASSERT_BOOL (is_op2const);
  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    m2statement_BuildAssignmentTree (
        location, m2treelib_get_rvalue (location, op1, type, is_op1lvalue),
        (*unop) (location,
                 m2treelib_get_rvalue (location, op2, type, is_op2lvalue),
                 false));
  else
    {
      /* Large set size > TSIZE(WORD).  */
      unsigned int fieldNo = 0;
      tree field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
      tree field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);

      if (is_op1const)
        error ("internal error: not expecting operand1 to be a constant set");

      while (field1 != NULL && field2 != NULL)
        {
          m2statement_BuildAssignmentTree (
              location, m2treelib_get_set_field_des (location, op1, field1),
              (*unop) (location,
                       m2treelib_get_set_field_rhs (location, op2, field2),
                       false));
          fieldNo++;
          field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
          field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);
        }
    }
}

/* BuildExcludeVarConst - builds the EXCL(op1, 1<<op2) operation for
   a small sets.  Large sets call this routine to exclude the bit in
   the particular word.  op2 is a constant.  */

void
m2statement_BuildExcludeVarConst (location_t location, tree type, tree op1,
                                  tree op2, bool is_lvalue, int fieldno)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);
  ASSERT_BOOL (is_lvalue);
  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    {
      /* Small set size <= TSIZE(WORD).  */
      m2statement_BuildAssignmentTree (
          location, m2treelib_get_rvalue (location, op1, type, is_lvalue),
          m2expr_BuildLogicalAnd (
              location, m2treelib_get_rvalue (location, op1, type, is_lvalue),
              m2expr_BuildSetNegate (
                  location,
                  m2expr_BuildLSL (location, m2expr_GetWordOne (location), op2,
                                   false),
                  false),
              false));
    }
  else
    {
      tree fieldlist = TYPE_FIELDS (type);
      tree field;

      for (field = fieldlist; (field != NULL) && (fieldno > 0);
           field = TREE_CHAIN (field))
        fieldno--;

      m2statement_BuildAssignmentTree (
          location, m2treelib_get_set_field_des (location, op1, field),
          m2expr_BuildLogicalAnd (
              location, m2treelib_get_set_field_rhs (location, op1, field),
              m2expr_BuildSetNegate (
                  location,
                  m2expr_BuildLSL (location, m2expr_GetWordOne (location), op2,
                                   false),
                  false),
              false));
    }
}

/* BuildExcludeVarVar - builds the EXCL(varset, 1<<varel) operation
   for a small and large sets.  varel is a variable.  */

void
m2statement_BuildExcludeVarVar (location_t location, tree type, tree varset,
                                tree varel, bool is_lvalue, tree low)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);
  ASSERT_BOOL (is_lvalue);
  /* Calculate the index from the first bit, ie bit 0 represents low value.  */
  tree index
      = m2expr_BuildSub (location, m2convert_ToInteger (location, varel),
                         m2convert_ToInteger (location, low), false);

  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    m2statement_BuildAssignmentTree (
        location, m2treelib_get_rvalue (location, varset, type, is_lvalue),
        m2expr_BuildLogicalAnd (
            location, m2treelib_get_rvalue (location, varset, type, is_lvalue),
            m2expr_BuildSetNegate (
                location,
                m2expr_BuildLSL (location, m2expr_GetWordOne (location),
                                 m2convert_ToWord (location, index), false),
                false),
            false));
  else
    {
      tree p1 = m2treelib_get_set_address (location, varset, is_lvalue);
      /* Calculate the index from the first bit.  */

      /* Which word do we need to fetch?  */
      tree word_index = m2expr_BuildDivTrunc (
          location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE), false);
      /* Calculate the bit in this word.  */
      tree offset_into_word = m2expr_BuildModTrunc (
          location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE), false);

      tree v1;

      /* Calculate the address of the word we are interested in.  */
      p1 = m2expr_BuildAddAddress (
          location, m2convert_convertToPtr (location, p1),
          m2expr_BuildMult (
              location, word_index,
              m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT),
              false));

      v1 = m2expr_BuildLogicalAnd (
          location,
          m2expr_BuildIndirect (location, p1, m2type_GetBitsetType ()),
          m2expr_BuildSetNegate (
              location,
              m2expr_BuildLSL (location, m2expr_GetWordOne (location),
                               m2convert_ToWord (location, offset_into_word),
                               false),
              false),
          false);

      /* Set bit offset_into_word within the word pointer at by p1.  */
      m2statement_BuildAssignmentTree (
          location,
          m2expr_BuildIndirect (location, p1, m2type_GetBitsetType ()),
          m2convert_ToBitset (location, v1));
    }
}

/* BuildIncludeVarConst - builds the INCL(op1, 1<<op2) operation for
   a small sets.  Large sets call this routine to include the bit in
   the particular word.  op2 is a constant.  */

void
m2statement_BuildIncludeVarConst (location_t location, tree type, tree op1,
                                  tree op2, bool is_lvalue, int fieldno)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);
  ASSERT_BOOL (is_lvalue);
  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    {
      /* Small set size <= TSIZE(WORD).  */
      m2statement_BuildAssignmentTree (
          location, m2treelib_get_rvalue (location, op1, type, is_lvalue),
          m2expr_BuildLogicalOr (
              location, m2treelib_get_rvalue (location, op1, type, is_lvalue),
              m2expr_BuildLSL (location, m2expr_GetWordOne (location),
                               m2convert_ToWord (location, op2), false),
              false));
    }
  else
    {
      tree fieldlist = TYPE_FIELDS (type);
      tree field;

      for (field = fieldlist; (field != NULL) && (fieldno > 0);
           field = TREE_CHAIN (field))
        fieldno--;

      m2statement_BuildAssignmentTree (
          location,
          /* Would like to use: m2expr_BuildComponentRef (location, p, field)
             but strangely we have to take the address of the field and
             dereference it to satify the gimplifier.  See
             testsuite/gm2/pim/pass/timeio?.mod for testcases.  */
          m2treelib_get_set_field_des (location, op1, field),
          m2expr_BuildLogicalOr (
              location, m2treelib_get_set_field_rhs (location, op1, field),
              m2expr_BuildLSL (location, m2expr_GetWordOne (location),
                               m2convert_ToWord (location, op2), false),
              false));
    }
}

/* BuildIncludeVarVar - builds the INCL(varset, 1<<varel) operation
   for a small and large sets.  op2 is a variable.  */

void
m2statement_BuildIncludeVarVar (location_t location, tree type, tree varset,
                                tree varel, bool is_lvalue, tree low)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);
  ASSERT_BOOL (is_lvalue);
  /* Calculate the index from the first bit, ie bit 0 represents low value.  */
  tree index
      = m2expr_BuildSub (location, m2convert_ToInteger (location, varel),
                         m2convert_ToInteger (location, low), false);
  tree indexw = m2convert_ToWord (location, index);

  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    m2statement_BuildAssignmentTree (
        location, m2treelib_get_rvalue (location, varset, type, is_lvalue),
        m2convert_ToBitset (
            location,
            m2expr_BuildLogicalOr (
                location,
                m2treelib_get_rvalue (location, varset, type, is_lvalue),
                m2expr_BuildLSL (location, m2expr_GetWordOne (location),
                                 indexw, false),
                false)));
  else
    {
      tree p1 = m2treelib_get_set_address (location, varset, is_lvalue);
      /* Which word do we need to fetch?  */
      tree word_index = m2expr_BuildDivTrunc (
          location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE), false);
      /* Calculate the bit in this word.  */
      tree offset_into_word = m2convert_BuildConvert (
          location, m2type_GetWordType (),
          m2expr_BuildModTrunc (location, index,
                                m2decl_BuildIntegerConstant (SET_WORD_SIZE),
                                false),
          false);
      tree v1;

      /* Calculate the address of the word we are interested in.  */
      p1 = m2expr_BuildAddAddress (
          location, m2convert_convertToPtr (location, p1),
          m2expr_BuildMult (
              location, word_index,
              m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT),
              false));
      v1 = m2expr_BuildLogicalOr (
          location,
          m2expr_BuildIndirect (location, p1, m2type_GetBitsetType ()),
          m2convert_ToBitset (location,
                              m2expr_BuildLSL (location,
                                               m2expr_GetWordOne (location),
                                               offset_into_word, false)),
          false);

      /* Set bit offset_into_word within the word pointer at by p1.  */
      m2statement_BuildAssignmentTree (
          location,
          m2expr_BuildIndirect (location, p1, m2type_GetBitsetType ()),
          m2convert_ToBitset (location, v1));
    }
}

/* BuildStart - creates a module initialization function.  We make
   this function public if it is not an inner module.  The linker
   will create a call list for all linked modules which determines
   the initialization sequence for all modules.  */

tree
m2statement_BuildStart (location_t location, char *name, bool inner_module)
{
  tree fntype;
  tree fndecl;

  m2assert_AssertLocation (location);
  /* The function type depends on the return type and type of args.  */
  fntype = build_function_type (integer_type_node, NULL_TREE);
  fndecl = build_decl (location, FUNCTION_DECL, get_identifier (name), fntype);

  DECL_EXTERNAL (fndecl) = 0;
  if (inner_module)
    TREE_PUBLIC (fndecl) = 0;
  else
    TREE_PUBLIC (fndecl) = 1;

  TREE_STATIC (fndecl) = 1;
  DECL_RESULT (fndecl)
      = build_decl (location, RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  /* Prevent the optimizer from removing it if it is public.  */
  if (TREE_PUBLIC (fndecl))
    gm2_mark_addressable (fndecl);

  m2statement_BuildStartFunctionCode (location, fndecl, !inner_module,
                                      inner_module);
  return fndecl;
}

/* BuildEnd - complete the initialization function for this module.  */

void
m2statement_BuildEnd (location_t location, tree fndecl, bool nested)
{
  m2statement_BuildEndFunctionCode (location, fndecl, nested);
  current_function_decl = NULL;
  set_cfun (NULL);
}

/* BuildCallInner - call the inner module function.  It has no
   parameters and no return value.  */

void
m2statement_BuildCallInner (location_t location, tree fndecl)
{
  m2assert_AssertLocation (location);
  param_list = NULL_TREE;
  add_stmt (location,
            m2statement_BuildProcedureCallTree (location, fndecl, NULL_TREE));
}


/* BuildIfThenDoEnd - returns a tree which will only execute
   statement, s, if, condition, is true.  */

tree
m2statement_BuildIfThenDoEnd (tree condition, tree then_block)
{
  if (then_block == NULL_TREE)
    return NULL_TREE;
  else
    return fold_build3 (COND_EXPR, void_type_node, condition, then_block,
                        alloc_stmt_list ());
}

/* BuildIfThenElseEnd - returns a tree which will execute then_block
   or else_block depending upon, condition.  */

tree
m2statement_BuildIfThenElseEnd (tree condition, tree then_block,
                                tree else_block)
{
  if (then_block == NULL_TREE)
    return NULL_TREE;
  else
    return fold_build3 (COND_EXPR, void_type_node, condition, then_block,
                        else_block);
}

/* BuildReturnValueCode - generates the code associated with: RETURN(
   value ) */

void
m2statement_BuildReturnValueCode (location_t location, tree fndecl, tree value)
{
  tree ret_stmt;
  tree t;

  m2assert_AssertLocation (location);
  t = build2 (
      MODIFY_EXPR, TREE_TYPE (DECL_RESULT (fndecl)), DECL_RESULT (fndecl),
      m2convert_BuildConvert (
          location, m2tree_skip_type_decl (TREE_TYPE (DECL_RESULT (fndecl))),
          value, false));

  ret_stmt = build_stmt (location, RETURN_EXPR, t);
  add_stmt (location, ret_stmt);
}

/* DoJump - jump to the appropriate label depending whether result of
   the expression is true or false.  */

void
m2statement_DoJump (location_t location, tree exp, char *falselabel,
                    char *truelabel)
{
  tree c = NULL_TREE;

  m2assert_AssertLocation (location);
  if (TREE_CODE (TREE_TYPE (exp)) != BOOLEAN_TYPE)
    exp = convert_loc (location, m2type_GetBooleanType (), exp);

  if ((falselabel != NULL) && (truelabel == NULL))
    {
      m2block_push_statement_list (m2block_begin_statement_list ());

      m2statement_BuildGoto (location, falselabel);
      c = build3 (COND_EXPR, void_type_node, exp,
                  m2block_pop_statement_list (),
                  alloc_stmt_list ());
    }
  else if ((falselabel == NULL) && (truelabel != NULL))
    {
      m2block_push_statement_list (m2block_begin_statement_list ());

      m2statement_BuildGoto (location, truelabel);
      c = build3 (COND_EXPR, void_type_node, exp,
                  m2block_pop_statement_list (),
                  alloc_stmt_list ());
    }
  else
    error_at (location, "expecting one and only one label to be declared");
  if (c != NULL_TREE)
    add_stmt (location, c);
}

#include "gt-m2-m2statement.h"
