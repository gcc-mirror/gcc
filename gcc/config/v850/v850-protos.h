/* Prototypes for v850.c functions used in the md file & elsewhere.
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.

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
   complications.  */
#ifndef GCC_V850_PROTOS_H
#define GCC_V850_PROTOS_H

#define Mmode enum machine_mode

extern void   expand_prologue               PARAMS ((void));
extern void   expand_epilogue               PARAMS ((void));
extern void   sdata_section                 PARAMS ((void));
extern void   rosdata_section               PARAMS ((void));
extern void   sbss_section                  PARAMS ((void));
extern void   tdata_section                 PARAMS ((void));
extern void   zdata_section                 PARAMS ((void));
extern void   rozdata_section               PARAMS ((void));
extern void   zbss_section                  PARAMS ((void));
extern int    v850_handle_pragma            PARAMS ((int (*)(void), void (*)(int), char *));
extern void   asm_file_start                PARAMS ((FILE *));
extern void   override_options              PARAMS ((void));
extern int    compute_register_save_size    PARAMS ((long *));
extern int    compute_frame_size            PARAMS ((int, long *));
extern void   v850_init_expanders           PARAMS ((void));

#ifdef RTX_CODE
extern int    v850_output_addr_const_extra  PARAMS ((FILE *, rtx));
extern rtx    v850_return_addr              PARAMS ((int));
extern void   print_operand                 PARAMS ((FILE *, rtx, int ));
extern void   print_operand_address         PARAMS ((FILE *, rtx));
extern int    const_costs                   PARAMS ((rtx, enum rtx_code));
extern const char *output_move_double       PARAMS ((rtx *));
extern const char *output_move_single       PARAMS ((rtx *));
extern void   v850_reorg                    PARAMS ((rtx));
extern void   notice_update_cc              PARAMS ((rtx, rtx));
extern char * construct_save_jarl           PARAMS ((rtx));
extern char * construct_restore_jr          PARAMS ((rtx));
#ifdef HAVE_MACHINE_MODES
extern int    reg_or_int9_operand           PARAMS ((rtx, Mmode));
extern int    reg_or_const_operand          PARAMS ((rtx, Mmode));
extern char * construct_dispose_instruction PARAMS ((rtx));
extern char * construct_prepare_instruction PARAMS ((rtx));
extern int    pattern_is_ok_for_prepare     PARAMS ((rtx, Mmode));
extern int    pattern_is_ok_for_dispose     PARAMS ((rtx, Mmode));
extern int    ep_memory_operand             PARAMS ((rtx, Mmode, int));
extern int    reg_or_0_operand              PARAMS ((rtx, Mmode));
extern int    reg_or_int5_operand           PARAMS ((rtx, Mmode));
extern int    call_address_operand          PARAMS ((rtx, Mmode));
extern int    movsi_source_operand          PARAMS ((rtx, Mmode));
extern int    power_of_two_operand          PARAMS ((rtx, Mmode));
extern int    not_power_of_two_operand      PARAMS ((rtx, Mmode));
extern int    special_symbolref_operand     PARAMS ((rtx, Mmode));
extern int    pattern_is_ok_for_prologue    PARAMS ((rtx, Mmode));
extern int    pattern_is_ok_for_epilogue    PARAMS ((rtx, Mmode));
extern int    register_is_ok_for_epilogue   PARAMS ((rtx, Mmode));
#ifdef TREE_CODE
extern rtx    function_arg                  PARAMS ((CUMULATIVE_ARGS *, Mmode, tree, int));
extern rtx    v850_va_arg                   PARAMS ((tree, tree));
#endif
#endif
#endif /* TREE_CODE */

#ifdef TREE_CODE
extern int    v850_interrupt_function_p     PARAMS ((tree));
extern void   v850_output_aligned_bss       PARAMS ((FILE *, tree, const char *, int, int));
extern void   v850_output_common            PARAMS ((FILE *, tree, const char *, int, int));
extern void   v850_output_local             PARAMS ((FILE *, tree, const char *, int, int));
extern v850_data_area v850_get_data_area    PARAMS ((tree));
#ifdef HAVE_MACHINE_MODES
extern int    function_arg_partial_nregs    PARAMS ((CUMULATIVE_ARGS *, Mmode, tree, int));
#endif
#endif

#ifdef GCC_C_PRAGMA_H
extern void ghs_pragma_section		    PARAMS ((cpp_reader *));
extern void ghs_pragma_interrupt	    PARAMS ((cpp_reader *));
extern void ghs_pragma_starttda		    PARAMS ((cpp_reader *));
extern void ghs_pragma_startsda		    PARAMS ((cpp_reader *));
extern void ghs_pragma_startzda		    PARAMS ((cpp_reader *));
extern void ghs_pragma_endtda		    PARAMS ((cpp_reader *));
extern void ghs_pragma_endsda		    PARAMS ((cpp_reader *));
extern void ghs_pragma_endzda		    PARAMS ((cpp_reader *));
#endif

#undef  Mmode

#endif /* ! GCC_V850_PROTOS_H */
