/* Configuration for GNU C-compiler for m68k Amiga, running AmigaOS.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2003
   Free Software Foundation, Inc.  
   Contributed by Markus M. Wild (wild@amiga.physik.unizh.ch).
   Heavily modified by Kamil Iskra (iskra@student.uci.agh.edu.pl).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#undef TARGET_AMIGAOS
#define TARGET_AMIGAOS 1

extern void amigaos_init_cumulative_args (CUMULATIVE_ARGS *, tree, tree);

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#undef INIT_CUMULATIVE_ARGS
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  (amigaos_init_cumulative_args(&(CUM), (FNTYPE), (INDIRECT)))


extern int amigaos_restore_a4 (void);
#ifdef RTX_CODE
extern int read_only_operand (rtx);
extern void amigaos_select_section (tree, int, unsigned HOST_WIDE_INT);
extern void amigaos_encode_section_info (tree, rtx, int);
extern void amigaos_alternate_pic_setup (FILE *);
extern void amigaos_prologue_begin_hook (FILE *, int);
extern void amigaos_alternate_frame_setup_f (FILE *, int);
extern void amigaos_alternate_frame_setup (FILE *, int);
extern struct rtx_def* gen_stack_cleanup_call (rtx, rtx);
extern void amigaos_alternate_allocate_stack (rtx *);
#ifdef TREE_CODE
//extern void amigaos_function_arg_advance (CUMULATIVE_ARGS *);
extern struct rtx_def *amigaos_function_arg (CUMULATIVE_ARGS *, enum machine_mode, tree);
#endif
#endif
#ifdef TREE_CODE
extern tree amigaos_handle_decl_attribute (tree *, tree, tree, int, bool *);
extern tree amigaos_handle_type_attribute (tree *, tree, tree, int, bool *);
#endif 

