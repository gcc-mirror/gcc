/* Prototypes for v850.c functions used in the md file & elsewhere.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* Function prototypes that cannot exist in v850.h due to dependency
   compilcations.  */
#define Mmode enum machine_mode

extern void   expand_prologue               PROTO ((void));
extern void   expand_epilogue               PROTO ((void));
extern void   sdata_section                 PROTO ((void));
extern void   rosdata_section               PROTO ((void));
extern void   sbss_section                  PROTO ((void));
extern void   tdata_section                 PROTO ((void));
extern void   zdata_section                 PROTO ((void));
extern void   rozdata_section               PROTO ((void));
extern void   zbss_section                  PROTO ((void));
extern int    v850_handle_pragma            PROTO ((int (*)(void), void (*)(int), char *));
extern void   asm_file_start                PROTO ((FILE *));
extern void   override_options              PROTO ((void));
extern int    compute_register_save_size    PROTO ((long *));
extern int    compute_frame_size            PROTO ((int, long *));

#ifdef RTX_CODE
extern void   print_operand                 PROTO ((FILE *, rtx, int ));
extern void   print_operand_address         PROTO ((FILE *, rtx));
extern int    const_costs                   PROTO ((rtx, enum rtx_code));
extern char * output_move_double            PROTO ((rtx *));
extern char * output_move_single            PROTO ((rtx *));
extern void   v850_reorg                    PROTO ((rtx));
extern void   notice_update_cc              PROTO ((rtx, rtx));
extern char * construct_save_jarl           PROTO ((rtx));
extern char * construct_restore_jr          PROTO ((rtx));
#ifdef HAVE_MACHINE_MODES
extern int    ep_memory_operand             PROTO ((rtx, Mmode, int));
extern int    reg_or_0_operand              PROTO ((rtx, Mmode));
extern int    reg_or_int5_operand           PROTO ((rtx, Mmode));
extern int    call_address_operand          PROTO ((rtx, Mmode));
extern int    movsi_source_operand          PROTO ((rtx, Mmode));
extern int    power_of_two_operand          PROTO ((rtx, Mmode));
extern int    not_power_of_two_operand      PROTO ((rtx, Mmode));
extern int    special_symbolref_operand     PROTO ((rtx, Mmode));
extern int    pattern_is_ok_for_prologue    PROTO ((rtx, Mmode));
extern int    pattern_is_ok_for_epilogue    PROTO ((rtx, Mmode));
extern int    register_is_ok_for_epilogue   PROTO ((rtx, Mmode));
#ifdef TREE_CODE
extern rtx    function_arg                  PROTO ((CUMULATIVE_ARGS *, Mmode, tree, int));
extern rtx    v850_va_arg                   PROTO ((tree, tree));
#endif
#endif
#endif /* TREE_CODE */

#ifdef TREE_CODE
extern int    v850_valid_machine_decl_attribute  PROTO ((tree, tree, tree));
extern void   v850_encode_data_area         PROTO ((tree));
extern void   v850_set_default_decl_attr    PROTO ((tree));
extern int    v850_interrupt_function_p     PROTO ((tree));
extern void   v850_output_aligned_bss       PROTO ((FILE *, tree, char *, int, int));
extern void   v850_output_common            PROTO ((FILE *, tree, char *, int, int));
extern void   v850_output_local             PROTO ((FILE *, tree, char *, int, int));
extern v850_data_area v850_get_data_area    PROTO ((tree));
#ifdef HAVE_MACHINE_MODES
extern int    function_arg_partial_nregs    PROTO ((CUMULATIVE_ARGS *, Mmode, tree, int));
#endif
#endif

#undef  Mmode

