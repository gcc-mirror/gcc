/* Subroutines for insn-output.c for GNU compiler.  Elxsi version.
   Copyright (C) 1987, 1992 Free Software Foundation, Inc
   This port, done by Mike Stump <mrs@cygnus.com> in 1988, and is the first
   64 bit port of GNU CC.
   Based upon the VAX port.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"

extern char *reg_names[];
rtx cmp_op0=0, cmp_op1=0;

/* table of relations for compares and branches */
char *cmp_tab[] = {
    "gt", "gt", "eq", "eq", "ge", "ge", "lt", "lt", "ne", "ne",
    "le", "le" };

/* type is the index into the above table */
/* s is "" for signed, or "u" for unsigned */
char *cmp_jmp(s, type, where) char *s; rtx where; {
    rtx br_ops[3];
    char template[50];
    char *f = "";
    char *bits = "64";
    if (GET_MODE (cmp_op0) == SFmode) f = "f", bits = "32";
    if (GET_MODE (cmp_op0) == DFmode) f = "f";
    br_ops[0] = where;
    br_ops[1] = cmp_op0;
    br_ops[2] = cmp_op1;
    if (cmp_op1)
	sprintf(template, "%scmp%s.br.%s\t%%1,%%2:j%s\t%%l0",
		f, s, bits, cmp_tab[type]);
    else if (*f)
	sprintf(template, "fcmp.br.%s\t%%1,=0:j%s\t%%l0",
		bits, cmp_tab[type]);
    else if (*s) /* can turn the below in to a jmp ... */
	sprintf(template, "cmpu.br.64\t%%1,=0:j%s\t%%l0", s, cmp_tab[type]);
    else
	sprintf(template, "jmp.%s\t%%1,%%l0", cmp_tab[type+1]);
    output_asm_insn(template, br_ops);
    return "";
}

char *cmp_set(s, type, reg) char *s, *type; rtx reg; {
    rtx br_ops[3];
    char template[50];
    char *f = "";
    char *bits = "64";
    if (GET_MODE (cmp_op0) == SFmode) f = "f", bits = "32";
    else if (GET_MODE (cmp_op0) == DFmode) f = "f";
    else if (GET_MODE (cmp_op0) == SImode) bits = "32";
    else if (GET_MODE (cmp_op0) == HImode) bits = "16";
    else if (GET_MODE (cmp_op0) == QImode) bits = "8";
    br_ops[0] = reg;
    br_ops[1] = cmp_op0;
    br_ops[2] = cmp_op1;
    if (cmp_op1)
	sprintf(template, "%scmp%s.%s\t%%0,%%1,%%2:%s",
		f, s, bits, type);
    else
	sprintf(template, "%scmp%s.%s\t%%0,%%1,=0:%s",
		f, s, bits, type);
    output_asm_insn(template, br_ops);
    return "";
}

print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  register rtx reg1, reg2, breg, ireg;
  rtx offset;

 retry:
  switch (GET_CODE (addr))
    {

    case MEM:
      if (GET_CODE (XEXP (addr, 0)) == REG)
        fprintf (file, "%s", reg_names[REGNO (addr)]);
      else abort();
      break;

    case REG:
      fprintf (file, "[%s]", reg_names[REGNO (addr)]);
      break;

    case PLUS:
      reg1 = 0;	reg2 = 0;
      ireg = 0;	breg = 0;
      offset = 0;
      if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  offset = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	{
	  offset = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      fprintf (file, "[%s]", reg_names[REGNO (addr)]);
      output_address (offset);
      break;

    default:
      output_addr_const (file, addr);
    }
}
