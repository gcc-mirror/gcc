/* Prototypes for exported functions defined in thumb.c
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Nick Clifton (nickc@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

extern int    thumb_shiftable_const	PARAMS ((HOST_WIDE_INT));
extern int    thumb_trivial_epilogue	PARAMS ((void));
extern void   thumb_finalize_pic	PARAMS ((void));
extern int    far_jump_used_p		PARAMS ((void));
extern char * output_return		PARAMS ((void));
extern void   thumb_function_prologue	PARAMS ((FILE *, int));
extern void   thumb_init_expanders	PARAMS ((void));
extern void   thumb_expand_prologue	PARAMS ((void));
extern void   thumb_expand_epilogue	PARAMS ((void));
extern void   thumb_function_epilogue	PARAMS ((FILE *, int));
extern char * thumb_unexpanded_epilogue	PARAMS ((void));
extern int    thumb_epilogue_size	PARAMS ((void));
extern void   thumb_override_options	PARAMS ((void));

#ifdef AOF_ASSEMBLER
extern char * aof_text_section		PARAMS ((int));
extern char * aof_data_section		PARAMS ((void));
extern void   thumb_aof_add_import	PARAMS ((char *));
extern void   thumb_aof_delete_import	PARAMS ((char *));
extern void   thumb_aof_dump_imports	PARAMS ((FILE *));
#endif /* AOF_ASSEMBLER */

#ifdef TREE_CODE
extern int    is_called_in_ARM_mode	PARAMS ((tree));
extern int    thumb_return_in_memory	PARAMS ((tree));
#ifdef THUMB_PE
extern int    arm_valid_machine_decl_attribute PARAMS ((tree, tree, tree));
#endif /* THUMB_PE */
#endif /* TREE_CODE */

#ifdef RTX_CODE
extern int    thumb_symbol_mentioned_p	PARAMS ((rtx));
extern int    label_mentioned_p		PARAMS ((rtx));
extern int    is_pic			PARAMS ((rtx));
extern void   thumb_reorg		PARAMS ((rtx));
extern rtx    thumb_return_addr		PARAMS ((int));
extern void   thumb_expand_movstrqi	PARAMS ((rtx *));
extern void   thumb_reload_out_si	PARAMS ((rtx));
extern void   thumb_final_prescan_insn	PARAMS ((rtx));
extern char * thumb_load_double_from_address PARAMS ((rtx *));
extern char * output_move_mem_multiple	PARAMS ((int, rtx *));
extern void   thumb_print_operand	PARAMS ((FILE *, rtx, int));

#ifdef HAVE_MACHINE_MODES
extern int    reload_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int    thumb_cmp_operand		PARAMS ((rtx, enum machine_mode));
extern rtx    legitimize_pic_address	PARAMS ((rtx, enum machine_mode, rtx));
#endif /* HAVE_MACHINE_MODES */
#endif /* RTX_CODE */
