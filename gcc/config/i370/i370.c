/* Subroutines for insn-output.c for System/370.
   Copyright (C) 1989, 1993, 1995 Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for MVS C/370 by Dave Pitts (dpitts@nyx.cs.du.edu)

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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"


/* Label node, this structure is used to keep track of labels on the
   current page.  */
typedef struct label_node
  {
    struct label_node *label_next;
    int label_id;
    int label_page;
  }
label_node_t;

/* Is 1 when a label has been generated and the base register must be
   reloaded.  */
int mvs_label_emitted = 0;

/* Current function starting base page.  */
int function_base_page;

/* Length of the current page code.  */
int mvs_page_code;

/* Length of the current page literals.  */
int mvs_page_lit;

/* Current function name.  */
char *mvs_function_name = 0;

/* Current function name length.  */
int mvs_function_name_length = 0;

/* Page number for multi-page functions.  */
int mvs_page_num = 0;

/* Label node list anchor.  */
static label_node_t *label_anchor = 0;

/* Label node free list anchor.  */
static label_node_t *free_anchor = 0;

/* Assembler source file descriptor.  */
static FILE *assembler_source = 0;

/* Define the length of the internal MVS function table.  */
#define MVS_FUNCTION_TABLE_LENGTH 32

/* C/370 internal function table.  These functions use non-standard linkage
   and must handled in a special manner.  */
static char *mvs_function_table[MVS_FUNCTION_TABLE_LENGTH] =
{
   "ceil",     "edc_acos", "edc_asin", "edc_ata2", "edc_atan", "edc_cos",
   "edc_cosh", "edc_erf",  "edc_erfc", "edc_exp",  "edc_gamm", "edc_lg10",
   "edc_log",  "edc_sin",  "edc_sinh", "edc_sqrt", "edc_tan",  "edc_tanh",
   "fabs",     "floor",    "fmod",     "frexp",    "hypot",    "j0",
   "j1",       "jn",       "ldexp",    "modf",     "pow",      "y0",
   "y1",       "yn"
};

/* ASCII to EBCDIC conversion table.  */
#if defined(TARGET_EBCDIC) && !defined(HOST_EBCDIC)
static unsigned char ascebc[256] =
{
 /*00  NL    SH    SX    EX    ET    NQ    AK    BL */
     0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F,
 /*08  BS    HT    LF    VT    FF    CR    SO    SI */
     0x16, 0x05, 0x15, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
 /*10  DL    D1    D2    D3    D4    NK    SN    EB */
     0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26,
 /*18  CN    EM    SB    EC    FS    GS    RS    US */
     0x18, 0x19, 0x3F, 0x27, 0x1C, 0x1D, 0x1E, 0x1F,
 /*20  SP     !     "     #     $     %     &     ' */
     0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D,
 /*28   (     )     *     +     ,     -    .      / */
     0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,
 /*30   0     1     2     3     4     5     6     7 */
     0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
 /*38   8     9     :     ;     <     =     >     ? */
     0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,
 /*40   @     A     B     C     D     E     F     G */
     0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
 /*48   H     I     J     K     L     M     N     O */
     0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,
 /*50   P     Q     R     S     T     U     V     W */
     0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6,
 /*58   X     Y     Z     [     \     ]     ^     _ */
     0xE7, 0xE8, 0xE9, 0xAD, 0xE0, 0xBD, 0x5F, 0x6D,
 /*60   `     a     b     c     d     e     f     g */
     0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
 /*68   h     i     j     k     l     m     n     o */
     0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,
 /*70   p     q     r     s     t     u     v     w */
     0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6,
 /*78   x     y     z     {     |     }     ~    DL */
     0xA7, 0xA8, 0xA9, 0xC0, 0x4F, 0xD0, 0xA1, 0x07,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0xFF
};
#endif

/* EBCDIC to ASCII conversion table.  */
#if defined(HOST_EBCDIC) && !defined(TARGET_EBCDIC)
unsigned char ebcasc[256] =
{
 /*00  NU    SH    SX    EX    PF    HT    LC    DL */
     0x00, 0x01, 0x02, 0x03, 0x00, 0x09, 0x00, 0x7F,
 /*08              SM    VT    FF    CR    SO    SI */
     0x00, 0x00, 0x00, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
 /*10  DE    D1    D2    TM    RS    NL    BS    IL */
     0x10, 0x11, 0x12, 0x13, 0x14, 0x0A, 0x08, 0x00,
 /*18  CN    EM    CC    C1    FS    GS    RS    US */
     0x18, 0x19, 0x00, 0x00, 0x1C, 0x1D, 0x1E, 0x1F,
 /*20  DS    SS    FS          BP    LF    EB    EC */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x0A, 0x17, 0x1B,
 /*28              SM    C2    EQ    AK    BL       */
     0x00, 0x00, 0x00, 0x00, 0x05, 0x06, 0x07, 0x00,
 /*30              SY          PN    RS    UC    ET */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04,
 /*38                    C3    D4    NK          SU */
     0x00, 0x00, 0x00, 0x00, 0x14, 0x15, 0x00, 0x1A,
 /*40  SP                                           */
     0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*48                     .     <     (     +     | */
     0x00, 0x00, 0x00, 0x2E, 0x3C, 0x28, 0x2B, 0x7C,
 /*50   &                                           */
     0x26, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*58               !     $     *     )     ;     ^ */
     0x00, 0x00, 0x21, 0x24, 0x2A, 0x29, 0x3B, 0x5E,
 /*60   -     /                                     */
     0x2D, 0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*68                     ,     %     _     >     ? */
     0x00, 0x00, 0x00, 0x2C, 0x25, 0x5F, 0x3E, 0x3F,
 /*70                                               */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*78         `     :     #     @     '     =     " */
     0x00, 0x60, 0x3A, 0x23, 0x40, 0x27, 0x3D, 0x22,
 /*80         a     b     c     d     e     f     g */
     0x00, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
 /*88   h     i           {                         */
     0x68, 0x69, 0x00, 0x7B, 0x00, 0x00, 0x00, 0x00,
 /*90         j     k     l     m     n     o     p */
     0x00, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70,
 /*98   q     r           }                         */
     0x71, 0x72, 0x00, 0x7D, 0x00, 0x00, 0x00, 0x00,
 /*A0         ~     s     t     u     v     w     x */
     0x00, 0x7E, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
 /*A8   y     z                       [             */
     0x79, 0x7A, 0x00, 0x00, 0x00, 0x5B, 0x00, 0x00,
 /*B0                                               */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*B8                                 ]             */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x5D, 0x00, 0x00,
 /*C0   {     A     B     C     D     E     F     G */
     0x7B, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
 /*C8   H     I                                     */
     0x48, 0x49, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*D0   }     J     K     L     M     N     O     P */
     0x7D, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50,
 /*D8   Q     R                                     */
     0x51, 0x52, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*E0   \           S     T     U     V     W     X */
     0x5C, 0x00, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
 /*E8   Y     Z                                     */
     0x59, 0x5A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*F0   0     1     2     3     4     5     6     7 */
     0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
 /*F8   8     9                                     */
     0x38, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF
};
#endif

/* Map characters from one character set to another.
   C is the character to be translated.  */

char
mvs_map_char (c)
     char c;
{
#if defined(TARGET_EBCDIC) && !defined(HOST_EBCDIC)
  return ascebc[c];
#else
#if defined(HOST_EBCDIC) && !defined(TARGET_EBCDIC)
  return ebcasc[c];
#else
  return c;
#endif
#endif
}

/* Emit reload of base register if indicated.  This is to eliminate multiple
   reloads when several labels are generated pointing to the same place
   in the code.  */

int
check_label_emit (void)
{
  if (mvs_label_emitted)
    {
      mvs_label_emitted = 0;
      mvs_page_code += 4;
      fprintf (assembler_source, "\tL\t%d,%d(,%d)\n",
	  BASE_REGISTER, (mvs_page_num - function_base_page) * 4,
	  PAGE_REGISTER);
    }
}

/* Add the label to the current page label list.  If a free element is available
   it will be used for the new label.  Otherwise, a label element will be
   allocated from memory.
   ID is the label number of the label being added to the list.  */

int
mvs_add_label (id)
     int id;
{
  label_node_t *lp;

  if (free_anchor)
    {
      lp = free_anchor;
      free_anchor = lp->label_next;
    }
  else
    {
      lp = (label_node_t *) malloc (sizeof (label_node_t));
      if (lp == 0)
	{
	  fatal ("virtual memory exhausted\n");
	  abort ();
	}
    }
  lp->label_id = id;
  lp->label_page = mvs_page_num;
  lp->label_next = label_anchor;
  label_anchor = lp;
}

/* Check to see if the label is in the list.  If 1 is returned then a load 
   and branch on register must be generated.
   ID is the label number of the label being checked.  */

int
mvs_check_label (id)
     int id;
{
  label_node_t *lp;

  for (lp = label_anchor; lp; lp = lp->label_next)
    {
      if (lp->label_id == id)
	return 1;
    }
  return 0;
}

/* The label list for the current page freed by linking the list onto the free
   label element chain.  */

int
mvs_free_label (void)
{
  if (label_anchor)
    {
      if (free_anchor)
	label_anchor->label_next = free_anchor;
      free_anchor = label_anchor;
    }
  label_anchor = 0;
}

/* If the page size limit is reached a new code page is started, and the base
   register is set to it.  This page break point is counted conservatively,
   most literals that have the same value are collapsed by the assembler.
   True is returned when a new page is started.
   FILE is the assembler output file descriptor.
   CODE is the length, in bytes, of the instruction to be emitted.
   LIT is the length of the literal to be emitted.  */

int
mvs_check_page (file, code, lit)
     FILE *file;
     int code, lit;
{
  if (file)
    assembler_source = file;

  if (mvs_page_code + code + mvs_page_lit + lit > MAX_MVS_PAGE_LENGTH)
    {
      fprintf (assembler_source, "\tB\tPGE%d\n", mvs_page_num);
      fprintf (assembler_source, "\tDS\t0F\n");
      fprintf (assembler_source, "\tLTORG\n");
      fprintf (assembler_source, "\tDS\t0F\n");
      fprintf (assembler_source, "PGE%d\tEQU\t*\n", mvs_page_num);
      fprintf (assembler_source, "\tDROP\t%d\n", BASE_REGISTER);
      mvs_page_num++;
      fprintf (assembler_source, "\tBALR\t%d,0\n", BASE_REGISTER);
      fprintf (assembler_source, "PG%d\tEQU\t*\n", mvs_page_num);
      fprintf (assembler_source, "\tUSING\t*,%d\n", BASE_REGISTER);
      mvs_free_label ();
      mvs_page_code = code;
      mvs_page_lit = lit;
      return 1;
    }
  mvs_page_code += code;
  mvs_page_lit += lit;
  return 0;
}

/* Check for C/370 runtime function, they don't use standard calling
   conventions.  True is returned if the function is in the table.
   NAME is the name of the current function.  */

int
mvs_function_check (name)
     char *name;
{
  int lower, middle, upper;
  int i;

  lower = 0;
  upper = MVS_FUNCTION_TABLE_LENGTH - 1;
  while (lower <= upper)
    {
      middle = (lower + upper) / 2;
      i = strcmp (name, mvs_function_table[middle]);
      if (i == 0)
	return 1;
      if (i < 0)
	upper = middle - 1;
      else
	lower = middle + 1;
    }
  return 0;
}


/* Return 1 if OP is a valid S operand for an RS, SI or SS type instruction.
   OP is the current operation.
   MODE is the current operation mode.  */

int
s_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  extern int volatile_ok;
  register enum rtx_code code = GET_CODE (op);

  if (CONSTANT_ADDRESS_P (op))
    return 1;
  if (mode == VOIDmode || GET_MODE (op) != mode)
    return 0;
  if (code == MEM)
    {
      register rtx x = XEXP (op, 0);

      if (!volatile_ok && op->volatil)
	return 0;
      if (REG_P (x) && REG_OK_FOR_BASE_P (x))
	return 1;
      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0)) && REG_OK_FOR_BASE_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && (unsigned) INTVAL (XEXP (x, 1)) < 4096)
	return 1;
    }
  return 0;
}


/* Return 1 if OP is a valid R or S operand for an RS, SI or SS type
   instruction.
   OP is the current operation.
   MODE is the current operation mode.  */

int
r_or_s_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  extern int volatile_ok;
  register enum rtx_code code = GET_CODE (op);

  if (CONSTANT_ADDRESS_P (op))
    return 1;
  if (mode == VOIDmode || GET_MODE (op) != mode)
    return 0;
  if (code == REG)
    return 1;
  else if (code == MEM)
    {
      register rtx x = XEXP (op, 0);

      if (!volatile_ok && op->volatil)
	return 0;
      if (REG_P (x) && REG_OK_FOR_BASE_P (x))
	return 1;
      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0)) && REG_OK_FOR_BASE_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && (unsigned) INTVAL (XEXP (x, 1)) < 4096)
	return 1;
    }
  return 0;
}


/* Return 1 if the next instruction is an unsigned jump instruction.
   INSN is the current instruction.  */

unsigned_jump_follows_p (insn)
     register rtx insn;
{
  insn = NEXT_INSN (insn);
  if (GET_CODE (insn) != JUMP_INSN)
    return 0;

  insn = XEXP (insn, 3);
  if (GET_CODE (insn) != SET)
    return 0;

  if (GET_CODE (XEXP (insn, 0)) != PC)
    return 0;

  insn = XEXP (insn, 1);
  if (GET_CODE (insn) != IF_THEN_ELSE)
    return 0;

  insn = XEXP (insn, 0);
  return GET_CODE (insn) != GE && GET_CODE (insn) != GT
    && GET_CODE (insn) != LE && GET_CODE (insn) != LT;
}
