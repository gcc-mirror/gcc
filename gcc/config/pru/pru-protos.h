/* Subroutine declarations for TI PRU target support.
   Copyright (C) 2014-2025 Free Software Foundation, Inc.
   Contributed by Dimitar Dimitrov <dimitar@dinux.eu>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_PRU_PROTOS_H
#define GCC_PRU_PROTOS_H

struct pru_ctable_entry {
    bool valid;
    unsigned HOST_WIDE_INT base;
};

extern struct pru_ctable_entry pru_ctable[32];

extern int pru_initial_elimination_offset (int, int);
extern int pru_can_use_return_insn (void);
extern void pru_expand_prologue (void);
extern void pru_expand_epilogue (bool);
extern void pru_function_profiler (FILE *, int);

void pru_register_pragmas (void);

#ifdef RTX_CODE
extern rtx pru_get_return_address (int);
extern int pru_hard_regno_rename_ok (unsigned int, unsigned int);

struct pru_byterange {
    int start;		/* Starting byte number.  */
    int nbytes;		/* Number of consecutive bytes.  */
};

extern pru_byterange pru_calc_byterange (HOST_WIDE_INT cval,
					      machine_mode mode);

extern const char *pru_output_signed_cbranch (rtx *, bool);
extern const char *pru_output_signed_cbranch_ubyteop2 (rtx *, bool);
extern const char *pru_output_signed_cbranch_zeroop2 (rtx *, bool);

extern enum rtx_code pru_noteq_condition (enum rtx_code code);
extern rtx pru_expand_fp_compare (rtx comparison, machine_mode mode);

extern void pru_emit_doloop (rtx *, int);

extern bool pru_regno_ok_for_base_p (int, bool);

static inline bool
pru_regno_ok_for_index_p (int regno, bool strict_p)
{
  /* Selection logic is the same - PRU instructions are quite orthogonal.  */
  return pru_regno_ok_for_base_p (regno, strict_p);
}

extern int pru_get_ctable_exact_base_index (unsigned HOST_WIDE_INT caddr);
extern int pru_get_ctable_base_index (unsigned HOST_WIDE_INT caddr);
extern int pru_get_ctable_base_offset (unsigned HOST_WIDE_INT caddr);

extern int pru_symref2ioregno (rtx op);

extern rtl_opt_pass *make_pru_tiabi_check (gcc::context *);
extern rtl_opt_pass *make_pru_minrt_check (gcc::context *);

#endif /* RTX_CODE */

#ifdef TREE_CODE
extern bool pru_return_in_memory (const_tree type, const_tree fntype);
#endif /* TREE_CODE */

#endif /* GCC_PRU_PROTOS_H */
