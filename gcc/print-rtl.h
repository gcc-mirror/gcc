/* Print RTL for GCC.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_PRINT_RTL_H
#define GCC_PRINT_RTL_H

/* A class for writing rtx to a FILE *.  */

class rtx_writer
{
 public:
  rtx_writer (FILE *outfile, int ind, bool simple, bool compact);

  void print_rtx (const_rtx in_rtx);
  void print_rtl (const_rtx rtx_first);
  int print_rtl_single_with_indent (const_rtx x, int ind);

 private:
  void print_rtx_operand_code_0 (const_rtx in_rtx, int idx);
  void print_rtx_operand_code_e (const_rtx in_rtx, int idx);
  void print_rtx_operand_codes_E_and_V (const_rtx in_rtx, int idx);
  void print_rtx_operand_code_i (const_rtx in_rtx, int idx);
  void print_rtx_operand_code_r (const_rtx in_rtx);
  void print_rtx_operand_code_u (const_rtx in_rtx, int idx);
  void print_rtx_operand (const_rtx in_rtx, int idx);
  bool operand_has_default_value_p (const_rtx in_rtx, int idx);

 private:
  FILE *m_outfile;
  int m_sawclose;
  int m_indent;
  bool m_in_call_function_usage;

  /* True means use simplified format without flags, modes, etc.  */
  bool m_simple;

  /* If true, use compact dump format:
     - PREV/NEXT_INSN UIDs are omitted
     - INSN_CODEs are omitted,
     - register numbers are omitted for hard and virtual regs, and
       non-virtual pseudos are offset relative to the first such reg, and
       printed with a '%' sigil e.g. "%0" for (LAST_VIRTUAL_REGISTER + 1),
     - insn names are prefixed with "c" (e.g. "cinsn", "cnote", etc).  */
  bool m_compact;
};

#ifdef BUFSIZ
extern void print_rtl (FILE *, const_rtx);
#endif
extern void print_rtx_insn_vec (FILE *file, const vec<rtx_insn *> &vec);

extern void dump_value_slim (FILE *, const_rtx, int);
extern void dump_insn_slim (FILE *, const rtx_insn *);
extern void dump_rtl_slim (FILE *, const rtx_insn *, const rtx_insn *,
			   int, int);
extern void print_value (pretty_printer *, const_rtx, int);
extern void print_pattern (pretty_printer *, const_rtx, int);
extern void print_insn (pretty_printer *pp, const rtx_insn *x, int verbose);

extern void rtl_dump_bb_for_graph (pretty_printer *, basic_block);
extern const char *str_pattern_slim (const_rtx);

extern void print_rtx_function (FILE *file, function *fn, bool compact);

#endif  // GCC_PRINT_RTL_H
