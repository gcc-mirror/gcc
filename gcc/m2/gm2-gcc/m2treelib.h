/* m2treelib.h header file for m2treelib.c.

Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

#if !defined(m2treelib_h)
#define m2treelib_h
#if defined(m2treelib_c)
#define EXTERN
#else /* !m2treelib_c.  */
#define EXTERN extern
#endif /* !m2treelib_c.  */

EXTERN void m2treelib_do_jump_if_bit (location_t location, enum tree_code code,
                                      tree word, tree bit, char *label);
EXTERN tree m2treelib_build_modify_expr (location_t location, tree des,
                                         enum tree_code modifycode, tree copy);
EXTERN tree m2treelib_DoCall (location_t location, tree rettype, tree funcptr,
                              tree param_list);
EXTERN tree m2treelib_DoCall0 (location_t location, tree rettype,
                               tree funcptr);
EXTERN tree m2treelib_DoCall1 (location_t location, tree rettype, tree funcptr,
                               tree arg0);
EXTERN tree m2treelib_DoCall2 (location_t location, tree rettype, tree funcptr,
                               tree arg0, tree arg1);
EXTERN tree m2treelib_DoCall3 (location_t location, tree rettype, tree funcptr,
                               tree arg0, tree arg1, tree arg2);
EXTERN tree m2treelib_get_rvalue (location_t location, tree t, tree type,
                                  int is_lvalue);
EXTERN tree m2treelib_get_field_no (tree type, tree op, int is_const,
                                    unsigned int fieldNo);
EXTERN tree m2treelib_get_set_value (location_t location, tree p, tree field,
                                     int is_const, int is_lvalue, tree op,
                                     unsigned int fieldNo);
EXTERN tree m2treelib_get_set_address (location_t location, tree op1,
                                       int is_lvalue);
EXTERN tree m2treelib_get_set_field_lhs (location_t location, tree p,
                                         tree field);
EXTERN tree m2treelib_get_set_field_rhs (location_t location, tree p,
                                         tree field);
EXTERN tree m2treelib_get_set_address_if_var (location_t location, tree op,
                                              int is_lvalue, int is_const);
EXTERN tree m2treelib_get_set_field_des (location_t location, tree p,
                                         tree field);

EXTERN tree add_stmt (location_t location, tree t);
EXTERN tree build_stmt (location_t loc, enum tree_code code, ...);

#undef EXTERN
#endif /* m2treelib_h.  */
