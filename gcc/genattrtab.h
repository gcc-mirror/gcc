/* External definitions of source files of genattrtab.
   Copyright (C)  2001 Free Software Foundation, Inc.

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

/* Defined in genattrtab.c: */
extern rtx check_attr_test	PARAMS ((rtx, int, int));
extern rtx make_numeric_value	PARAMS ((int));
extern void make_internal_attr	PARAMS ((const char *, rtx, int));
extern char *attr_printf	PARAMS ((unsigned int, const char *, ...))
  ATTRIBUTE_PRINTF_2;

extern int num_dfa_decls;

/* Defined in genautomata.c: */
extern void gen_cpu_unit		PARAMS ((rtx));
extern void gen_query_cpu_unit		PARAMS ((rtx));
extern void gen_bypass			PARAMS ((rtx));
extern void gen_excl_set		PARAMS ((rtx));
extern void gen_presence_set		PARAMS ((rtx));
extern void gen_absence_set		PARAMS ((rtx));
extern void gen_automaton		PARAMS ((rtx));
extern void gen_automata_option		PARAMS ((rtx));
extern void gen_reserv   		PARAMS ((rtx));
extern void gen_insn_reserv     	PARAMS ((rtx));
extern void initiate_automaton_gen	PARAMS ((int, char **));
extern void expand_automata             PARAMS ((void));
extern void write_automata              PARAMS ((void));
