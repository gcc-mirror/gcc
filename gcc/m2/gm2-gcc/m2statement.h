/* m2statement.h header file for m2statement.cc.

Copyright (C) 2012-2023 Free Software Foundation, Inc.
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

#if !defined(m2statement_h)
#define m2statement_h
#if defined(m2statement_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2statement_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2statement_c.  */

EXTERN void m2statement_BuildCallInner (location_t location, tree fndecl);
EXTERN void m2statement_BuildEnd (location_t location, tree fndecl,
                                  int nested);
EXTERN tree m2statement_BuildStart (location_t location, char *name,
                                    int inner_module);
EXTERN void m2statement_BuildIncludeVarVar (location_t location, tree type,
                                            tree varset, tree varel,
                                            bool is_lvalue, tree low);
EXTERN void m2statement_BuildIncludeVarConst (location_t location, tree type,
                                              tree op1, tree op2,
                                              bool is_lvalue, int fieldno);
EXTERN void m2statement_BuildExcludeVarVar (location_t location, tree type,
                                            tree varset, tree varel,
                                            bool is_lvalue, tree low);
EXTERN void m2statement_BuildExcludeVarConst (location_t location, tree type,
                                              tree op1, tree op2,
                                              bool is_lvalue, int fieldno);
EXTERN void m2statement_BuildUnaryForeachWordDo (
    location_t location, tree type, tree op1, tree op2,
    tree (*unop) (location_t, tree, bool), bool is_op1lvalue, bool is_op2lvalue,
    bool is_op1const, bool is_op2const);
EXTERN void m2statement_BuildAsm (location_t location, tree instr,
                                  bool isVolatile, bool isSimple, tree inputs,
                                  tree outputs, tree trash, tree labels);
EXTERN tree m2statement_BuildFunctValue (location_t location, tree value);
EXTERN tree m2statement_BuildIndirectProcedureCallTree (location_t location,
                                                        tree procedure,
                                                        tree rettype);
EXTERN tree m2statement_BuildProcedureCallTree (location_t location,
                                                tree procedure, tree rettype);
EXTERN void m2statement_BuildFunctionCallTree (location_t location,
					       tree procedure, tree rettype);
EXTERN void m2statement_BuildParam (location_t location, tree param);

EXTERN tree m2statement_BuildIfThenElseEnd (tree condition, tree then_block,
                                            tree else_block);
EXTERN tree m2statement_BuildIfThenDoEnd (tree condition, tree then_block);

EXTERN void m2statement_DeclareLabel (location_t location, char *name);
EXTERN void m2statement_BuildGoto (location_t location, char *name);
EXTERN tree m2statement_BuildAssignmentTree (location_t location, tree des,
                                             tree expr);
EXTERN void m2statement_BuildAssignmentStatement (location_t location, tree des,
						  tree expr);
EXTERN void m2statement_BuildPopFunctionContext (void);
EXTERN void m2statement_BuildPushFunctionContext (void);
EXTERN void m2statement_BuildReturnValueCode (location_t location, tree fndecl,
                                              tree value);
EXTERN void m2statement_BuildEndFunctionCode (location_t location, tree fndecl,
                                              bool nested);
EXTERN void m2statement_BuildStartFunctionCode (location_t location,
                                                tree fndecl, bool isexported,
                                                bool isinline);
EXTERN void m2statement_DoJump (location_t location, tree exp,
                                char *falselabel, char *truelabel);
EXTERN tree m2statement_BuildCall2 (location_t location, tree function,
                                    tree rettype, tree arg1, tree arg2);
EXTERN tree m2statement_BuildCall3 (location_t location, tree function,
                                    tree rettype, tree arg1, tree arg2,
                                    tree arg3);
EXTERN void m2statement_SetLastFunction (tree t);
EXTERN tree m2statement_GetLastFunction (void);
EXTERN void m2statement_SetParamList (tree t);
EXTERN tree m2statement_GetParamList (void);
EXTERN tree m2statement_GetCurrentFunction (void);
EXTERN void m2statement_SetBeginLocation (location_t location);
EXTERN void m2statement_SetEndLocation (location_t location);
EXTERN tree m2statement_GetParamTree (tree call, unsigned int i);
EXTERN tree m2statement_BuildTryFinally (location_t location, tree call,
                                         tree cleanups);
EXTERN tree m2statement_BuildCleanUp (tree param);

#undef EXTERN
#endif /* m2statement_h.  */
