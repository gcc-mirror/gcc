/* General-purpose hooks.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#ifndef GCC_HOOKS_H
#define GCC_HOOKS_H

bool hook_bool_void_false PARAMS ((void));

/* Check if tm.h has been included, since ISO C does not allow forward
   definitions for enums, and making hooks.h dependent on tm.h would create
   unnecessary dependencies where no hook declaration involving
   enum_reg_class is needed.  */
#ifdef REG_CLASS_CONTENTS
enum reg_class hook_reg_class_void_no_regs (void);
#endif

bool hook_bool_bool_false (bool);
bool hook_bool_tree_false PARAMS ((tree));
bool hook_bool_tree_hwi_hwi_tree_false
  PARAMS ((tree, HOST_WIDE_INT, HOST_WIDE_INT, tree));
bool hook_bool_tree_hwi_hwi_tree_true
  PARAMS ((tree, HOST_WIDE_INT, HOST_WIDE_INT, tree));
bool hook_bool_rtx_false PARAMS ((rtx));
bool hook_bool_rtx_int_int_intp_false PARAMS ((rtx, int, int, int *));

void hook_void_tree_int PARAMS ((tree, int));
void hook_void_void PARAMS ((void));
void hook_void_FILEptr_constcharptr PARAMS ((FILE *, const char *));
void hook_void_tree PARAMS ((tree));
void hook_void_tree_treeptr PARAMS ((tree, tree *));

int hook_int_tree_tree_1 PARAMS ((tree, tree));
int hook_int_rtx_0 PARAMS ((rtx));
int hook_int_void_0 (void);
int hook_int_size_t_constcharptr_int_0 (size_t, const char *, int);

bool default_can_output_mi_thunk_no_vcall
  PARAMS ((tree, HOST_WIDE_INT, HOST_WIDE_INT, tree));

bool hook_bool_tree_tree_false PARAMS ((tree, tree));

rtx hook_rtx_rtx_identity PARAMS ((rtx));
rtx hook_rtx_rtx_null PARAMS ((rtx));

#endif
