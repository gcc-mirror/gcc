/* Prototypes for exported functions defined in mcore.c
   Copyright (C) 2000-2013 Free Software Foundation, Inc.
   Contributed by Nick Clifton (nickc@redhat.com)

   This file is part of GCC.

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

extern const char * mcore_output_jump_label_table	(void);
extern void         mcore_expand_prolog          	(void);
extern void         mcore_expand_epilog          	(void);
extern int          mcore_const_ok_for_inline    	(HOST_WIDE_INT);
extern int          mcore_num_ones               	(HOST_WIDE_INT);
extern int          mcore_num_zeros              	(HOST_WIDE_INT);
extern int          mcore_initial_elimination_offset	(int, int);
extern int          mcore_byte_offset            	(unsigned int);
extern int          mcore_halfword_offset        	(unsigned int);
extern int          mcore_const_trick_uses_not   	(HOST_WIDE_INT);
extern int          mcore_dllexport_name_p       	(const char *);
extern int          mcore_dllimport_name_p       	(const char *);
extern int          mcore_naked_function_p       	(void);

#ifdef TREE_CODE
#ifdef HAVE_MACHINE_MODES
extern int          mcore_num_arg_regs           	(enum machine_mode, const_tree);
#endif /* HAVE_MACHINE_MODES */

#ifdef RTX_CODE
extern rtx          mcore_function_value         	(const_tree, const_tree);
#endif /* RTX_CODE */
#endif /* TREE_CODE */

#ifdef RTX_CODE

extern const char * mcore_output_bclri         		(rtx, int);
extern const char * mcore_output_bseti         		(rtx, int);
extern const char * mcore_output_cmov          		(rtx *, int, const char *);
extern char *       mcore_output_call          		(rtx *, int);
extern int          mcore_is_dead                	(rtx, rtx);
extern int          mcore_expand_insv            	(rtx *);
extern bool         mcore_expand_block_move      	(rtx *);
extern const char * mcore_output_andn          		(rtx, rtx *);
extern bool         mcore_gen_compare	        	(RTX_CODE, rtx, rtx);
extern int          mcore_symbolic_address_p     	(rtx);
extern bool         mcore_r15_operand_p			(rtx);
extern enum reg_class mcore_secondary_reload_class	(enum reg_class, enum machine_mode, rtx);
extern enum reg_class mcore_reload_class 		(rtx, enum reg_class);
extern int          mcore_is_same_reg            	(rtx, rtx);
extern int          mcore_arith_S_operand         	(rtx);

#ifdef HAVE_MACHINE_MODES
extern const char * mcore_output_move          		(rtx, rtx *, enum machine_mode);
extern const char * mcore_output_movedouble    		(rtx *, enum machine_mode);
extern int          const_ok_for_mcore                  (HOST_WIDE_INT);
#endif /* HAVE_MACHINE_MODES */
#endif /* RTX_CODE */
