/* External definitions of source files of genattrtab.
   Copyright (C)  2001, 2003, 2005 Free Software Foundation, Inc.

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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Name of function (attribute) to translate insn into number of insn
   alternatives reservation.  */
#define INSN_ALTS_FUNC_NAME "insn_alts"

/* Defined in genattrtab.c: */
extern rtx check_attr_test (rtx, int, int);
extern rtx make_numeric_value (int);
extern void make_internal_attr (const char *, rtx, int);
extern char *attr_printf (unsigned int, const char *, ...)
  ATTRIBUTE_PRINTF_2;

extern int num_dfa_decls;

/* Defined in genautomata.c: */
extern void gen_cpu_unit (rtx);
extern void gen_query_cpu_unit (rtx);
extern void gen_bypass (rtx);
extern void gen_excl_set (rtx);
extern void gen_presence_set (rtx);
extern void gen_final_presence_set (rtx);
extern void gen_absence_set (rtx);
extern void gen_final_absence_set (rtx);
extern void gen_automaton (rtx);
extern void gen_automata_option (rtx);
extern void gen_reserv (rtx);
extern void gen_insn_reserv (rtx);
extern void initiate_automaton_gen (int, char **);
extern void expand_automata (void);
extern void write_automata (void);

/* Flags for make_internal_attr's `special' parameter.  */
#define ATTR_NONE		0
#define ATTR_SPECIAL		(1 << 0)
#define ATTR_STATIC		(1 << 1)
