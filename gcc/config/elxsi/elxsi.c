/* Subroutines for insn-output.c for GNU compiler.  Elxsi version.
   Copyright (C) 1987, 1992, 1998, 1999, 2000 Free Software Foundation, Inc
   Contributrd by Mike Stump <mrs@cygnus.com> in 1988 and is the first
   64 bit port of GNU CC.
   Based upon the VAX port.

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "function.h"
#include "output.h"
#include "tree.h"
#include "expr.h"
#include "regs.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

extern const char *reg_names[];
rtx cmp_op0=0, cmp_op1=0;

/* table of relations for compares and branches */
static const char *const cmp_tab[] = {
    "gt", "gt", "eq", "eq", "ge", "ge", "lt", "lt", "ne", "ne",
    "le", "le" };

static bool elxsi_assemble_integer PARAMS ((rtx, unsigned int, int));
static void elxsi_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void elxsi_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP NULL
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP NULL
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER elxsi_assemble_integer

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE elxsi_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE elxsi_output_function_epilogue

struct gcc_target targetm = TARGET_INITIALIZER;

/* Target hook for assembling integer objects.  The ELXSI assembler
   syntax uses a suffix to indicate the size of data, so we can't use
   the usual string hooks.  */

static bool
elxsi_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
  if (aligned_p)
    switch (size)
      {
      case 1:
      case 2:
      case 4:
	fputs ("\t.data\t", asm_out_file);
	output_addr_const (asm_out_file, x);
	fprintf (asm_out_file, "{%d}\n", size * BITS_PER_UNIT);
	return true;
      }
  return default_assemble_integer (x, size, aligned_p);
}

/* Generate the assembly code for function entry.  FILE is a stdio
   stream to output the code to.  SIZE is an int: how many units of
   temporary storage to allocate.

   Refer to the array `regs_ever_live' to determine which registers to
   save; `regs_ever_live[I]' is nonzero if register number I is ever
   used in the function.  This function is responsible for knowing
   which registers should not be saved even if used.  */

static void
elxsi_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  register int regno;
  register int cnt = 0;

  /* the below two lines are a HACK, and should be deleted, but
     for now are very much needed (1.35) */
  if (frame_pointer_needed)
    regs_ever_live[14] = 1, call_used_regs[14] = 0;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (regs_ever_live[regno] && !call_used_regs[regno])
      cnt += 8;

  if (size + cnt)
    fprintf (file, "\tadd.64\t.sp,=%d\n", -size - cnt);

  cnt = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (regs_ever_live[regno] && !call_used_regs[regno])
      fprintf (file, "\tst.64\t.r%d,[.sp]%d\n", regno, (cnt += 8) - 12);

  if (frame_pointer_needed)
    fprintf (file, "\tadd.64\t.r14,.sp,=%d\n", size + cnt);
}

/* This function generates the assembly code for function exit.
   Args are as for output_function_prologue ().

   The function epilogue should not depend on the current stack
   pointer!  It should use the frame pointer only.  This is mandatory
   because of alloca; we also take advantage of it to omit stack
   adjustments before returning. */

static void
elxsi_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  register int regno;
  register int cnt = 0;

  /* this conditional is ONLY here because there is a BUG;
     EXIT_IGNORE_STACK is ignored itself when the first part of
     the condition is true! (at least in version 1.35) */
  /* the 8*10 is for 64 bits of .r5 - .r14 */
  if (current_function_calls_alloca || size >= (256 - 8 * 10))
    {
      /* use .r4 as a temporary! Ok for now.... */
      fprintf (file, "\tld.64\t.r4,.r14\n");

      for (regno = FIRST_PSEUDO_REGISTER-1; regno >= 0; --regno)
	if (regs_ever_live[regno] && !call_used_regs[regno])
	  cnt += 8;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	if (regs_ever_live[regno] && !call_used_regs[regno])
	  fprintf (file, "\tld.64\t.r%d,[.r14]%d\n", regno,
		   -((cnt -= 8) + 8) - 4 - size);

      fprintf (file, "\tld.64\t.sp,.r4\n\texit\t0\n");
    }
  else
    {
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	if (regs_ever_live[regno] && !call_used_regs[regno])
	  fprintf (file, "\tld.64\t.r%d,[.sp]%d\n", regno, (cnt += 8) - 12);

      fprintf (file, "\texit\t%d\n", size + cnt);
    }
}

/* type is the index into the above table */
/* s is "" for signed, or "u" for unsigned */
const char *
cmp_jmp (s, type, where)
     const char *s;
     int type;
     rtx where;
{
    rtx br_ops[3];
    char template[50];
    const char *f = "";
    const char *bits = "64";
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
	sprintf(template, "cmpu.br.64\t%%1,=0:j%s\t%%l0", s);
    else
	sprintf(template, "jmp.%s\t%%1,%%l0", cmp_tab[type+1]);
    output_asm_insn(template, br_ops);
    return "";
}

const char *
cmp_set (s, type, reg)
     const char *s, *type;
     rtx reg;
{
    rtx br_ops[3];
    char template[50];
    const char *f = "";
    const char *bits = "64";
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

void
print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  register rtx reg1, reg2, breg, ireg;
  rtx offset;

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
