/* Subroutines for insn-output.c for System/370.
   Copyright (C) 1989, 1993, 1995, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for OS/390 LanguageEnvironment C by Dave Pitts (dpitts@cozx.com)
   Hacked for Linux-ELF/390 by Linas Vepstas (linas@linas.org) 

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
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "function.h"
#include "flags.h"
#include "recog.h"
#include "toplev.h"
#include "tm_p.h"

extern FILE *asm_out_file;

/* Label node.  This structure is used to keep track of labels 
      on the various pages in the current routine.
   The label_id is the numeric ID of the label,
   The label_page is the page on which it actually appears,
   The first_ref_page is the page on which the true first ref appears.
   The label_addr is an estimate of its location in the current routine,
   The label_first & last_ref are estimates of where the earliest and
      latest references to this label occur.                                     */

typedef struct label_node
  {
    struct label_node *label_next;
    int label_id;
    int label_page;
    int first_ref_page;

    int label_addr;
    int label_first_ref;
    int label_last_ref;
  }
label_node_t;

/* Is 1 when a label has been generated and the base register must be reloaded.  */
int mvs_need_base_reload = 0;

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

static label_node_t * mvs_get_label PARAMS ((int));
static void i370_label_scan PARAMS ((void));

/* ===================================================== */
/* defines and functions specific to the HLASM assembler */
#ifdef TARGET_HLASM

#ifndef MAX_MVS_LABEL_SIZE
#define MAX_MVS_LABEL_SIZE 8
#endif

#define MAX_LONG_LABEL_SIZE 255

/* Alias node, this structure is used to keep track of aliases to external
   variables. The IBM assembler allows an alias to an external name 
   that is longer that 8 characters; but only once per assembly.
   Also, this structure stores the #pragma map info.  */
typedef struct alias_node
  {
    struct alias_node *alias_next;
    int  alias_emitted;
    char alias_name [MAX_MVS_LABEL_SIZE + 1];
    char real_name [MAX_LONG_LABEL_SIZE + 1];
  }
alias_node_t;

/* Alias node list anchor.  */
static alias_node_t *alias_anchor = 0;

/* Alias number */
#ifdef LONGEXTERNAL
static int alias_number = 0;
#endif

/* Define the length of the internal MVS function table.  */
#define MVS_FUNCTION_TABLE_LENGTH 32

/* C/370 internal function table.  These functions use non-standard linkage
   and must handled in a special manner.  */
static const char *const mvs_function_table[MVS_FUNCTION_TABLE_LENGTH] =
{
#if defined(HOST_EBCDIC) /* Changed for EBCDIC collating sequence */
   "ceil",     "edc_acos", "edc_asin", "edc_atan", "edc_ata2", "edc_cos",
   "edc_cosh", "edc_erf",  "edc_erfc", "edc_exp",  "edc_gamm", "edc_lg10",
   "edc_log",  "edc_sin",  "edc_sinh", "edc_sqrt", "edc_tan",  "edc_tanh",
   "fabs",     "floor",    "fmod",     "frexp",    "hypot",    "jn",
   "j0",       "j1",       "ldexp",    "modf",     "pow",      "yn",
   "y0",       "y1"
#else
   "ceil",     "edc_acos", "edc_asin", "edc_ata2", "edc_atan", "edc_cos",
   "edc_cosh", "edc_erf",  "edc_erfc", "edc_exp",  "edc_gamm", "edc_lg10",
   "edc_log",  "edc_sin",  "edc_sinh", "edc_sqrt", "edc_tan",  "edc_tanh",
   "fabs",     "floor",    "fmod",     "frexp",    "hypot",    "j0",
   "j1",       "jn",       "ldexp",    "modf",     "pow",      "y0",
   "y1",       "yn"
#endif
};

#endif /* TARGET_HLASM */
/* ===================================================== */

/* ASCII to EBCDIC conversion table.  */
static const unsigned char ascebc[256] =
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

/* EBCDIC to ASCII conversion table.  */
static const unsigned char ebcasc[256] =
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

/* Map characters from one character set to another.
   C is the character to be translated.  */

char
mvs_map_char (c)
     int c;
{
#if defined(TARGET_EBCDIC) && !defined(HOST_EBCDIC)
  fprintf (stderr, "mvs_map_char: TE & !HE: c = %02x\n", c);
  return ascebc[c];
#else
#if defined(HOST_EBCDIC) && !defined(TARGET_EBCDIC)
  fprintf (stderr, "mvs_map_char: !TE & HE: c = %02x\n", c);
  return ebcasc[c];
#else
  fprintf (stderr, "mvs_map_char: !TE & !HE: c = %02x\n", c);
  return c;
#endif
#endif
}

/* ===================================================== */
/* The following three routines are used to determine whther 
   forward branch is on this page, or is a far jump.  We use
   the "length" attr on an insn [(set_atter "length" "4")]
   to store the largest possible code length that insn
   could have.  This gives us a hint of the address of a
   branch destination, and from that, we can work out 
   the length of the jump, and whether its on page or not. 
 */

/* Return the destination address of a branch.  */

int
i370_branch_dest (branch)
     rtx branch;
{
  rtx dest = SET_SRC (PATTERN (branch));
  int dest_uid;
  int dest_addr;

  /* first, compute the estimated address of the branch target */
  if (GET_CODE (dest) == IF_THEN_ELSE)
    dest = XEXP (dest, 1);
  dest = XEXP (dest, 0);
  dest_uid = INSN_UID (dest);
  dest_addr =  insn_addresses[dest_uid];

  /* next, record the address of this insn as the true addr of first ref */
  {
     label_node_t *lp;
     rtx label = JUMP_LABEL (branch);
     int labelno = CODE_LABEL_NUMBER (label);

     if (!label || CODE_LABEL != GET_CODE (label)) abort ();

     lp = mvs_get_label (labelno);
     if (-1 == lp -> first_ref_page) lp->first_ref_page = mvs_page_num;
  }
  return dest_addr;
}

int
i370_branch_length (insn)
     rtx insn;
{
  int here, there;
  here = insn_addresses[INSN_UID (insn)];
  there = i370_branch_dest (insn);
  return (there - here);
}


int
i370_short_branch (insn)
     rtx insn;
{
  int base_offset;

  base_offset = i370_branch_length(insn);
  if (0 > base_offset) 
    {
      base_offset += mvs_page_code;
    } 
  else 
    {
      /* avoid bumping into lit pool; use 2x to estimate max possible lits */
      base_offset *= 2;
      base_offset += mvs_page_code + mvs_page_lit;
    }
  
  /* make a conservative estimate of room left on page */
  if ((4060 >base_offset) && ( 0 < base_offset)) return 1;
  return 0;
}

/* The i370_label_scan() routine is supposed to loop over
   all labels and label references in a compilation unit,
   and determine whether all label refs appear on the same 
   code page as the label. If they do, then we can avoid 
   a reload of the base register for that label.
  
   Note that the instruction addresses used here are only 
   approximate, and make the sizes of the jumps appear
   farther apart then they will actually be.  This makes 
   this code far more conservative than it needs to be.
 */

#define I370_RECORD_LABEL_REF(label,addr) {				\
	label_node_t *lp;						\
	int labelno = CODE_LABEL_NUMBER (label);			\
	lp = mvs_get_label (labelno);					\
	if (addr < lp -> label_first_ref) lp->label_first_ref = addr;	\
	if (addr > lp -> label_last_ref) lp->label_last_ref = addr;	\
}

void 
i370_label_scan () 
{
   rtx insn;
   label_node_t *lp;
   int tablejump_offset = 0;

   for (insn = get_insns(); insn; insn = NEXT_INSN(insn))
     {
       int here = insn_addresses[INSN_UID (insn)];
       enum rtx_code code = GET_CODE(insn);

       /* ??? adjust for tables embedded in the .text section that
        * the compiler didn't take into account */
       here += tablejump_offset;
       insn_addresses[INSN_UID (insn)] = here;

       /* check to see if this insn is a label ... */
       if (CODE_LABEL == code)
         {
           int labelno = CODE_LABEL_NUMBER (insn);

           lp = mvs_get_label (labelno);
           lp -> label_addr = here;
#if 0
           /* Supposedly, labels are supposed to have circular
              lists of label-refs that reference them, 
              setup in flow.c, but this does not appear to be the case.  */
           rtx labelref = LABEL_REFS (insn);
           rtx ref = labelref;
           do 
             {
               rtx linsn = CONTAINING_INSN(ref);
               ref =  LABEL_NEXTREF(ref);
             } while (ref && (ref != labelref));
#endif
         }
       else
       if (JUMP_INSN == code)
         {
           rtx label = JUMP_LABEL (insn);

           /* If there is no label for this jump, then this
              had better be a ADDR_VEC or an ADDR_DIFF_VEC
              and there had better be a vector of labels.   */
           if (!label) 
             {
               int j;
               rtx body = PATTERN (insn);
               if (ADDR_VEC == GET_CODE(body)) 
                 {
                    for (j=0; j < XVECLEN (body, 0); j++)
                      {
                         rtx lref = XVECEXP (body, 0, j);
                         if (LABEL_REF != GET_CODE (lref)) abort ();
                         label = XEXP (lref,0);
                         if (CODE_LABEL != GET_CODE (label)) abort ();
                         tablejump_offset += 4;
                         here += 4;
                         I370_RECORD_LABEL_REF(label,here);
                      }
                    /* finished with the vector go do next insn */
                    continue;
                 }
               else
               if (ADDR_DIFF_VEC == GET_CODE(body))
                 {
/* XXX hack alert.
   Right now, we leave this as a no-op, but strictly speaking,
   this is incorrect.  It is possible that a table-jump
   driven off of a relative address could take us off-page,
   to a place where we need to reload the base reg.  So really,
   we need to examing both labels, and compare thier values
   to the current basereg value.
  
   More generally, this brings up a troubling issue overall:
   what happens if a tablejump is split across two pages? I do 
   not beleive that this case is handled correctly at all, and
   can only lead to horrible results if this were to occur.
  
   However, the current situation is not any worse than it was 
   last week, and so we punt for now.  */

                    debug_rtx (insn);
                    for (j=0; j < XVECLEN (body, 0); j++)
                      {
                      }
                    /* finished with the vector go do next insn */
                    continue;
                 }
               else 
                 {
/* XXX hack alert.
   Compiling the execption handling (L_eh) in libgcc2.a will trip
   up right here, with something that looks like
   (set (pc) (mem:SI (plus:SI (reg/v:SI 1 r1) (const_int 4))))
      {indirect_jump} 
   I'm not sure of what leads up to this, but it looks like
   the makings of a long jump which will surely get us into trouble
   because the base & page registers don't get reloaded.  For now
   I'm not sure of what to do ... again we punt ... we are not worse
   off than yesterday.  */

                    /* print_rtl_single (stdout, insn); */
                    debug_rtx (insn);
                    /* abort(); */
                    continue;
                 }
            }
          else
            {
              /* At this point, this jump_insn had better be a plain-old
                 ordinary one, grap the label id and go */
              if (CODE_LABEL != GET_CODE (label)) abort ();
              I370_RECORD_LABEL_REF(label,here);
            }
        }

      /* Sometimes, we take addresses of labels and use them
         as instruction operands ... these show up as REG_NOTES */
      else
      if (INSN == code)
       {
         if ('i' == GET_RTX_CLASS (code)) 
           {
              rtx note;
              for (note = REG_NOTES (insn); note;  note = XEXP(note,1))
                {
                   if (REG_LABEL == REG_NOTE_KIND(note))
                     {
                        rtx label = XEXP (note,0);
                        if (!label || CODE_LABEL != GET_CODE (label)) abort ();

                        I370_RECORD_LABEL_REF(label,here);
                     }
                }
           }
       }
   }
}

/* ===================================================== */

/* Emit reload of base register if indicated.  This is to eliminate multiple
   reloads when several labels are generated pointing to the same place
   in the code.  

   The page table is written at the end of the function. 
   The entries in the page table look like
     .LPGT0:          // PGT0 EQU *
     .long .LPG0      // DC A(PG0)
     .long .LPG1      // DC A(PG1)
  while the prologue generates
      L       r4,=A(.LPGT0)

  Note that this paging scheme breaks down if a single subroutine 
  has more than about 10MB of code in it ... as long as humans write
  code, this shouldn't be a problem ...
 */

void
check_label_emit ()
{
  if (mvs_need_base_reload)
    {
      mvs_need_base_reload = 0;

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

label_node_t *
mvs_get_label (id)
     int id;
{
  label_node_t *lp;

  /* first, lets see if we already go one, if so, use that. */
  for (lp = label_anchor; lp; lp = lp->label_next)
    {
      if (lp->label_id == id) return lp;
    }

  /* not found, get a new one */
  if (free_anchor)
    {
      lp = free_anchor;
      free_anchor = lp->label_next;
    }
  else
    {
      lp = (label_node_t *) xmalloc (sizeof (label_node_t));
    }

  /* initialize for new label */
  lp->label_id = id;
  lp->label_page = -1;
  lp->label_next = label_anchor;
  lp->label_first_ref = 2000123123;
  lp->label_last_ref = -1;
  lp->label_addr = -1;
  lp->first_ref_page = -1;
  label_anchor = lp;

  return lp;
}

void
mvs_add_label (id)
     int id;
{
  label_node_t *lp;
  int fwd_distance;

  lp = mvs_get_label (id);
  lp->label_page = mvs_page_num;

  /* OK, we just saw the label.  Determine if this label
   * needs a reload of the base register */
  if ((-1 != lp->first_ref_page) && 
      (lp->first_ref_page != mvs_page_num)) 
    {
      /* Yep; the first label_ref was on a different page. */
      mvs_need_base_reload ++;
      return;
    }

  /* Hmm.  Try to see if the estimated address of the last
     label_ref is on the current page.  If it is, then we
     don't need a base reg reload.  Note that this estimate
     is very conservatively handled; we'll tend to have 
     a good bit more reloads than actually needed.  Someday,
     we should tighten the estimates (which are driven by
     the (set_att "length") insn attibute.
    
     Currently, we estimate that number of page literals 
     same as number of insns, which is a vast overestimate,
     esp that the estimate of each insn size is its max size.  */

  /* if latest ref comes before label, we are clear */
  if (lp->label_last_ref < lp->label_addr) return;

  fwd_distance = lp->label_last_ref - lp->label_addr;

  if (mvs_page_code + 2 * fwd_distance + mvs_page_lit < 4060) return;

  mvs_need_base_reload ++;
}

/* Check to see if the label is in the list and in the current
   page.  If not found, we have to make worst case assumption 
   that label will be on a different page, and thus will have to
   generate a load and branch on register.  This is rather
   ugly for forward-jumps, but what can we do? For backward
   jumps on the same page we can branch directly to address.
   ID is the label number of the label being checked.  */

int
mvs_check_label (id)
     int id;
{
  label_node_t *lp;

  for (lp = label_anchor; lp; lp = lp->label_next)
    {
      if (lp->label_id == id) 
        {
          if (lp->label_page == mvs_page_num) 
            {
               return 1;
            } 
          else 
            {
	       return 0;
            } 
        }
    }
  return 0;
}

/* Get the page on which the label sits.  This will be used to 
   determine is a register reload is really needed.  */

int
mvs_get_label_page(int id)
{
  label_node_t *lp;

  for (lp = label_anchor; lp; lp = lp->label_next)
    {
      if (lp->label_id == id)
	return lp->label_page;
    }
  return -1;
}

/* The label list for the current page freed by linking the list onto the free
   label element chain.  */

void
mvs_free_label_list ()
{

  if (label_anchor)
    {
      label_node_t *last_lp = label_anchor;
      while (last_lp->label_next) last_lp = last_lp->label_next;
      last_lp->label_next = free_anchor;
      free_anchor = label_anchor;
    }
  label_anchor = 0;
}

/* ====================================================================== */
/* If the page size limit is reached a new code page is started, and the base
   register is set to it.  This page break point is counted conservatively,
   most literals that have the same value are collapsed by the assembler.
   True is returned when a new page is started.
   FILE is the assembler output file descriptor.
   CODE is the length, in bytes, of the instruction to be emitted.
   LIT is the length of the literal to be emitted.  */

#ifdef TARGET_HLASM
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
      /* Safe to use BASR not BALR, since we are
       * not switching addressing mode here ... */
      fprintf (assembler_source, "\tBASR\t%d,0\n", BASE_REGISTER);
      fprintf (assembler_source, "PG%d\tEQU\t*\n", mvs_page_num);
      fprintf (assembler_source, "\tUSING\t*,%d\n", BASE_REGISTER);
      mvs_page_code = code;
      mvs_page_lit = lit;
      return 1;
    }
  mvs_page_code += code;
  mvs_page_lit += lit;
  return 0;
}
#endif /* TARGET_HLASM */


#ifdef TARGET_ELF_ABI
int
mvs_check_page (file, code, lit)
     FILE *file;
     int code, lit;
{
  if (file)
    assembler_source = file;

  if (mvs_page_code + code + mvs_page_lit + lit > MAX_MVS_PAGE_LENGTH)
    {
      /* hop past the literal pool */
      fprintf (assembler_source, "\tB\t.LPGE%d\n", mvs_page_num);

      /* dump the literal pool. The .baligns are optional, since 
       * ltorg will align to the size of the largest literal 
       * (which is possibly 8 bytes) */
      fprintf (assembler_source, "\t.balign\t4\n");
      fprintf (assembler_source, "\t.LTORG\n");
      fprintf (assembler_source, "\t.balign\t4\n");

      /* we continue execution here ... */
      fprintf (assembler_source, ".LPGE%d:\n", mvs_page_num);
      fprintf (assembler_source, "\t.DROP\t%d\n", BASE_REGISTER);
      mvs_page_num++;

      /* BASR puts the contents of the PSW into r3
       * that is, r3 will be loaded with the address of "." */
      fprintf (assembler_source, "\tBASR\tr%d,0\n", BASE_REGISTER);
      fprintf (assembler_source, ".LPG%d:\n", mvs_page_num);
      fprintf (assembler_source, "\t.USING\t.,r%d\n", BASE_REGISTER);
      mvs_page_code = code;
      mvs_page_lit = lit;
      return 1;
    }
  mvs_page_code += code;
  mvs_page_lit += lit;
  return 0;
}
#endif /* TARGET_ELF_ABI */

/* ===================================================== */
/* defines and functions specific to the HLASM assembler */
#ifdef TARGET_HLASM

/* Check for C/370 runtime function, they don't use standard calling
   conventions.  True is returned if the function is in the table.
   NAME is the name of the current function.  */

int
mvs_function_check (name)
     const char *name;
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


/* Add the alias to the current alias list.  */

void
mvs_add_alias (realname, aliasname, emitted)
     const char *realname;
     const char *aliasname;
     int emitted;
{
  alias_node_t *ap;

  ap = (alias_node_t *) xmalloc (sizeof (alias_node_t));
  strcpy (ap->real_name, realname);
  strcpy (ap->alias_name, aliasname);
  ap->alias_emitted = emitted;
  ap->alias_next = alias_anchor;
  alias_anchor = ap;
}

/* Check to see if the name needs aliasing */

int
mvs_need_alias (realname)
      const char *realname;
{
   if (mvs_function_check (realname))
     return 0;
   if (strlen (realname) > MAX_MVS_LABEL_SIZE)
     return 1;
   if (strchr (realname, '_') != 0)
     return 1;
   return 0;
}

/* Get the alias from the list. 
   If 1 is returned then it's in the alias list, 0 if it was not */

int
mvs_get_alias (realname, aliasname)
     const char *realname;
     char *aliasname;
{
#ifdef LONGEXTERNAL
  alias_node_t *ap;

  for (ap = alias_anchor; ap; ap = ap->alias_next)
    {
      if (!strcmp (ap->real_name, realname))
	{
	  strcpy (aliasname, ap->alias_name);
	  return 1;
	}
    }
  if (mvs_need_alias (realname))
    {
      sprintf (aliasname, "ALS%05d", alias_number++);
      mvs_add_alias (realname, aliasname, 0);
      return 1;
    }
#else
  if (strlen (realname) > MAX_MVS_LABEL_SIZE)
    {
      strncpy (aliasname, realname, MAX_MVS_LABEL_SIZE);
      aliasname[MAX_MVS_LABEL_SIZE] = '\0';
      return 1;
    }
#endif
  return 0;
}

/* Check to see if the alias is in the list. 
   If 1 is returned then it's in the alias list, 2 it was emitted  */

int
mvs_check_alias (realname, aliasname)
     const char *realname;
     char *aliasname;
{
#ifdef LONGEXTERNAL
  alias_node_t *ap;

  for (ap = alias_anchor; ap; ap = ap->alias_next)
    {
      if (!strcmp (ap->real_name, realname))
	{
	  int rc = (ap->alias_emitted == 1) ? 1 : 2; 
	  strcpy (aliasname, ap->alias_name);
	  ap->alias_emitted = 1; 
	  return rc;
	}
    }
  if (mvs_need_alias (realname))
    {
      sprintf (aliasname, "ALS%05d", alias_number++);
      mvs_add_alias (realname, aliasname, 0);
      alias_anchor->alias_emitted = 1;
      return 2;
    }
#else
  if (strlen (realname) > MAX_MVS_LABEL_SIZE)
    {
      strncpy (aliasname, realname, MAX_MVS_LABEL_SIZE);
      aliasname[MAX_MVS_LABEL_SIZE] = '\0';
      return 1;
    }
#endif
  return 0;
}

/* Called from check_newline via the macro HANDLE_PRAGMA.
   FINPUT is the source file input stream.
   NODE is the tree node for the token after the "pragma".
   The result is 1 if the pragma was handled.  */

int
handle_pragma (p_getc, p_ungetc, pname)
     int (* p_getc) PARAMS ((void));
     void (* p_ungetc) PARAMS ((int));
     const char *pname;
{
  int retval = 0;
  register int c;

  if (strcmp (pname, "map") == 0)
    {
      char realname[MAX_LONG_LABEL_SIZE + 1];
      char aliasname[MAX_MVS_LABEL_SIZE + 1];
      char *s;

      do {
	c = p_getc ();
      } while (c == ' ' || c == '\t');

      if (c == '(')
        {
	  s = realname;
	  do {
	    c = p_getc ();
	  } while (c == ' ' || c == '\t');
	  if (c == '\n')
	    goto PRAGMA_WARNING;
	  do {
	    *s++ = c;
	    c = p_getc ();
	  } while (ISALNUM(c) || c == '_');
	  if (c == '\n')
	    goto PRAGMA_WARNING;
	  *s = 0;

	  if (c == ' ' || c == '\t')
	    do {
	      c = p_getc ();
	    } while (c == ' ' || c == '\t');
	  
	  if (c == ',')
	    {
	      do {
	        c = p_getc ();
	      } while (c == ' ' || c == '\t');
	      if (c == '"')
	        {
	          s = aliasname;
	          c = p_getc ();
	          do {
	            if (c == '\\')
	              {
	                int d = 0;
	                do {
	                  c = p_getc ();
	                  if (c >= '0' && c <= '7')
	                      d = (d << 3) | (c - '0');
	                } while (c >= '0' && c <= '7');
	                p_ungetc (c);
	                c = d;
	                if (d < 1 || d > 255)
			  warning ("Escape value out of range");
#ifndef HOST_EBCDIC
                        c = ebcasc[c];
#endif
	              }
	            *s++ = c;
	            c = p_getc ();
	            if (ISSPACE(c) || c == ')')
	              goto PRAGMA_WARNING;
	          } while (c != '"');
	          *s = 0;
		  if (strlen (aliasname) > MAX_MVS_LABEL_SIZE)
		    {
		      warning ("#pragma map alias is too long, truncated");
		      aliasname[MAX_MVS_LABEL_SIZE] = '\0';
		    }
		  do {
		    c = p_getc ();
		  } while (c == ' ' || c == '\t');
		  if (c == ')')
		    {
	              mvs_add_alias (realname, aliasname, 1);
		      retval = 1;
		    }
	          else
	            goto PRAGMA_WARNING;
	        }
	      else
	        goto PRAGMA_WARNING;
	    }
	  else
	    goto PRAGMA_WARNING;
	  
        }
      else
        {
	 PRAGMA_WARNING:
	  warning ("#pragma map options are missing or incorrect");
        }
      
    }

  return retval;
}

/* defines and functions specific to the HLASM assembler */
#endif /* TARGET_HLASM */
/* ===================================================== */
/* ===================================================== */
/* defines and functions specific to the gas assembler */
#ifdef TARGET_ELF_ABI

/* Check for C/370 runtime function, they don't use standard calling
   conventions.  True is returned if the function is in the table.
   NAME is the name of the current function.  */
/* no special calling conventions (yet ??) */

int
mvs_function_check (name)
     const char *name ATTRIBUTE_UNUSED;
{
   return 0;
}

#endif /* TARGET_ELF_ABI */
/* ===================================================== */


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


/* Some remarks about unsigned_jump_follows_p():
   gcc is built around the assumption that branches are signed
   or unsigned, whereas the 370 doesn't care; its the compares that
   are signed or unsigned.  Thus, we need to somehow know if we
   need to do a signed or an unsigned compare, and we do this by 
   looking ahead in the instruction sequence until we find a jump.
   We then note whether this jump is signed or unsigned, and do the 
   compare appropriately.  Note that we have to scan ahead indefinitley,
   as the gcc optimizer may insert any number of instructions between 
   the compare and the jump.
  
   Note that using conditional branch expanders seems to be be a more 
   elegant/correct way of doing this.   See, for instance, the Alpha 
   cmpdi and bgt patterns.  Note also that for the i370, various
   arithmetic insn's set the condition code as well.

   The unsigned_jump_follows_p() routine  returns a 1 if the next jump 
   is unsigned.  INSN is the current instruction.  */

int
unsigned_jump_follows_p (insn)
     register rtx insn;
{
  rtx orig_insn = insn;
  while (1) 
    {
      register rtx tmp_insn;
      enum rtx_code coda;
  
      insn = NEXT_INSN (insn);
      if (!insn) fatal_insn ("internal error--no jump follows compare:", orig_insn);
  
      if (GET_CODE (insn) != JUMP_INSN) continue;
    
      tmp_insn = XEXP (insn, 3);
      if (GET_CODE (tmp_insn) != SET) continue;
    
      if (GET_CODE (XEXP (tmp_insn, 0)) != PC) continue;
    
      tmp_insn = XEXP (tmp_insn, 1);
      if (GET_CODE (tmp_insn) != IF_THEN_ELSE) continue;
    
      /* if we got to here, this instruction is a jump.  Is it signed? */
      tmp_insn = XEXP (tmp_insn, 0);
      coda = GET_CODE (tmp_insn);
  
      return coda != GE && coda != GT && coda != LE && coda != LT;
    }
}


#ifdef TARGET_HLASM

void
i370_function_prolog (f, l)
     FILE *f;
     int l;
{
#if MACROPROLOGUE == 1
  fprintf (f, "* Function %s prologue\n", mvs_function_name);
  fprintf (f, "\tEDCPRLG USRDSAL=%d,BASEREG=%d\n",
	   STACK_POINTER_OFFSET + l - 120 +
	   current_function_outgoing_args_size, BASE_REGISTER);
#else /* MACROPROLOGUE != 1 */
  static int function_label_index = 1;
  static int function_first = 0;
  static int function_year, function_month, function_day;
  static int function_hour, function_minute, function_second;
#if defined(LE370)
  if (!function_first)
    {
      struct tm *function_time;
      time_t lcltime;
      time (&lcltime);
      function_time = localtime (&lcltime);
      function_year = function_time->tm_year + 1900;
      function_month = function_time->tm_mon + 1;
      function_day = function_time->tm_mday;
      function_hour = function_time->tm_hour;
      function_minute = function_time->tm_min;
      function_second = function_time->tm_sec;
    }
  fprintf (f, "* Function %s prologue\n", mvs_function_name);
  fprintf (f, "FDSE%03d\tDSECT\n", function_label_index);
  fprintf (f, "\tDS\tD\n");
  fprintf (f, "\tDS\tCL(%d)\n", STACK_POINTER_OFFSET + l
			+ current_function_outgoing_args_size);
  fprintf (f, "\tORG\tFDSE%03d\n", function_label_index);
  fprintf (f, "\tDS\tCL(120+8)\n");
  fprintf (f, "\tORG\n");
  fprintf (f, "\tDS\t0D\n");
  fprintf (f, "FDSL%03d\tEQU\t*-FDSE%03d-8\n", function_label_index,
	   function_label_index);
  fprintf (f, "\tDS\t0H\n");
  assemble_name (f, mvs_function_name);
  fprintf (f, "\tCSECT\n");
  fprintf (f, "\tUSING\t*,15\n");
  fprintf (f, "\tB\tFENT%03d\n", function_label_index);
  fprintf (f, "\tDC\tAL1(FNAM%03d+4-*)\n", function_label_index);
  fprintf (f, "\tDC\tX'CE',X'A0',AL1(16)\n");
  fprintf (f, "\tDC\tAL4(FPPA%03d)\n", function_label_index);
  fprintf (f, "\tDC\tAL4(0)\n");
  fprintf (f, "\tDC\tAL4(FDSL%03d)\n", function_label_index);
  fprintf (f, "FNAM%03d\tEQU\t*\n", function_label_index);
  fprintf (f, "\tDC\tAL2(%d),C'%s'\n", strlen (mvs_function_name),
	mvs_function_name);
  fprintf (f, "FPPA%03d\tDS\t0F\n", function_label_index);
  fprintf (f, "\tDC\tX'03',X'00',X'33',X'00'\n");
  fprintf (f, "\tDC\tV(CEESTART)\n");
  fprintf (f, "\tDC\tAL4(0)\n");
  fprintf (f, "\tDC\tAL4(FTIM%03d)\n", function_label_index);
  fprintf (f, "FTIM%03d\tDS\t0F\n", function_label_index);
  fprintf (f, "\tDC\tCL4'%d',CL4'%02d%02d',CL6'%02d%02d00'\n",
  		 function_year, function_month, function_day,
    		 function_hour, function_minute);
  fprintf (f, "\tDC\tCL2'01',CL4'0100'\n");
  fprintf (f, "FENT%03d\tDS\t0H\n", function_label_index);
  fprintf (f, "\tSTM\t14,12,12(13)\n");
  fprintf (f, "\tL\t2,76(,13)\n");
  fprintf (f, "\tL\t0,16(,15)\n");
  fprintf (f, "\tALR\t0,2\n");
  fprintf (f, "\tCL\t0,12(,12)\n");
  fprintf (f, "\tBNH\t*+10\n");
  fprintf (f, "\tL\t15,116(,12)\n");
  fprintf (f, "\tBALR\t14,15\n");
  fprintf (f, "\tL\t15,72(,13)\n");
  fprintf (f, "\tSTM\t15,0,72(2)\n");
  fprintf (f, "\tMVI\t0(2),X'10'\n");
  fprintf (f, "\tST\t2,8(,13)\n ");
  fprintf (f, "\tST\t13,4(,2)\n ");
  fprintf (f, "\tLR\t13,2\n");
  fprintf (f, "\tDROP\t15\n");
  fprintf (f, "\tBALR\t%d,0\n", BASE_REGISTER);
  fprintf (f, "\tUSING\t*,%d\n", BASE_REGISTER);
  function_first = 1;
  function_label_index ++;
#else /* !LE370 */
  if (!function_first)
    {
      struct tm *function_time;
      time_t lcltime;
      time (&lcltime);
      function_time = localtime (&lcltime);
      function_year = function_time->tm_year + 1900;
      function_month = function_time->tm_mon + 1;
      function_day = function_time->tm_mday;
      function_hour = function_time->tm_hour;
      function_minute = function_time->tm_min;
      function_second = function_time->tm_sec;
      fprintf (f, "PPA2\tDS\t0F\n");
      fprintf (f, "\tDC\tX'03',X'00',X'33',X'00'\n");
      fprintf (f, "\tDC\tV(CEESTART),A(0)\n");
      fprintf (f, "\tDC\tA(CEETIMES)\n");
      fprintf (f, "CEETIMES\tDS\t0F\n");
      fprintf (f, "\tDC\tCL4'%d',CL4'%02d%02d',CL6'%02d%02d00'\n",
    		 function_year, function_month, function_day,
    		 function_hour, function_minute, function_second);
      fprintf (f, "\tDC\tCL2'01',CL4'0100'\n");
    }
  fprintf (f, "* Function %s prologue\n", mvs_function_name);
  fprintf (f, "FDSD%03d\tDSECT\n", function_label_index);
  fprintf (f, "\tDS\tD\n");
  fprintf (f, "\tDS\tCL(%d)\n", STACK_POINTER_OFFSET + l
			+ current_function_outgoing_args_size);
  fprintf (f, "\tORG\tFDSD%03d\n", function_label_index);
  fprintf (f, "\tDS\tCL(120+8)\n");
  fprintf (f, "\tORG\n");
  fprintf (f, "\tDS\t0D\n");
  fprintf (f, "FDSL%03d\tEQU\t*-FDSD%03d-8\n", function_label_index,
	   function_label_index);
  fprintf (f, "\tDS\t0H\n");
  assemble_name (f, mvs_function_name);
  fprintf (f, "\tCSECT\n");
  fprintf (f, "\tUSING\t*,15\n");
  fprintf (f, "\tB\tFPL%03d\n", function_label_index);
  fprintf (f, "\tDC\tAL1(FPL%03d+4-*)\n", function_label_index + 1);
  fprintf (f, "\tDC\tX'CE',X'A0',AL1(16)\n");
  fprintf (f, "\tDC\tAL4(PPA2)\n");
  fprintf (f, "\tDC\tAL4(0)\n");
  fprintf (f, "\tDC\tAL4(FDSL%03d)\n", function_label_index);
  fprintf (f, "FPL%03d\tEQU\t*\n", function_label_index + 1);
  fprintf (f, "\tDC\tAL2(%d),C'%s'\n", strlen (mvs_function_name),
	mvs_function_name);
  fprintf (f, "FPL%03d\tDS\t0H\n", function_label_index);
  fprintf (f, "\tSTM\t14,12,12(13)\n");
  fprintf (f, "\tL\t2,76(,13)\n");
  fprintf (f, "\tL\t0,16(,15)\n");
  fprintf (f, "\tALR\t0,2\n");
  fprintf (f, "\tCL\t0,12(,12)\n");
  fprintf (f, "\tBNH\t*+10\n");
  fprintf (f, "\tL\t15,116(,12)\n");
  fprintf (f, "\tBALR\t14,15\n");
  fprintf (f, "\tL\t15,72(,13)\n");
  fprintf (f, "\tSTM\t15,0,72(2)\n");
  fprintf (f, "\tMVI\t0(2),X'10'\n");
  fprintf (f, "\tST\t2,8(,13)\n ");
  fprintf (f, "\tST\t13,4(,2)\n ");
  fprintf (f, "\tLR\t13,2\n");
  fprintf (f, "\tDROP\t15\n");
  fprintf (f, "\tBALR\t%d,0\n", BASE_REGISTER);
  fprintf (f, "\tUSING\t*,%d\n", BASE_REGISTER);
  function_first = 1;
  function_label_index += 2;
#endif /* !LE370 */
#endif /* MACROPROLOGUE */
  fprintf (f, "PG%d\tEQU\t*\n", mvs_page_num );
  fprintf (f, "\tLR\t11,1\n"); 
  fprintf (f, "\tL\t%d,=A(PGT%d)\n", PAGE_REGISTER, mvs_page_num);
  fprintf (f, "* Function %s code\n", mvs_function_name);

  mvs_free_label_list ();
  mvs_page_code = 6;
  mvs_page_lit = 4;
  mvs_check_page (f, 0, 0);
  function_base_page = mvs_page_num;

  /* find all labels in this routine */
  i370_label_scan ();
}
#endif /* TARGET_HLASM */


#ifdef TARGET_ELF_ABI
/*
   The 370_function_prolog() routine generates the current ELF ABI ES/390 prolog.
   It implements a stack that grows downward. 
   It performs the following steps:
   -- saves the callers non-volatile registers on the callers stack.
   -- subtracts stackframe size from the stack pointer.
   -- stores backpointer to old caller stack.
  
   XXX hack alert -- if the global var int leaf_function is non-zero, 
   then this is a leaf, and it might be possible to optimize the prologue
   into doing even less, e.g. not grabbing a new stackframe or maybe just a
   partial stack frame.
  
   XXX hack alert -- the current stack frame is bloated into twice the 
   needed size by unused entries. These entries make it marginally 
   compatible with MVS/OE/USS C environment, but really they're not used
   and could probably chopped out. Modifications to i370.md would be needed
   also, to quite using addresses 136, 140, etc.
 */

void
i370_function_prolog (f, frame_size)
     FILE *f;
     int frame_size;
{
  static int function_label_index = 1;
  static int function_first = 0;
  int stackframe_size, aligned_size;

  fprintf (f, "# Function prologue\n");
  /* define the stack, put it into its own data segment
     FDSE == Function Stack Entry
     FDSL == Function Stack Length */
  stackframe_size = 
     STACK_POINTER_OFFSET + current_function_outgoing_args_size + frame_size;
  aligned_size = (stackframe_size + 7) >> 3;
  aligned_size <<= 3;
  
  fprintf (f, "# arg_size=0x%x frame_size=0x%x aligned size=0x%x\n", 
     current_function_outgoing_args_size, frame_size, aligned_size);

  fprintf (f, "\t.using\t.,r15\n");

  /* Branch to exectuable part of prologue. */
  fprintf (f, "\tB\t.LFENT%03d\n", function_label_index);

  /* write the length of the stackframe */
  fprintf (f, "\t.long\t%d\n", aligned_size);

  /* FENT == function prologue entry */
  fprintf (f, "\t.balign 2\n.LFENT%03d:\n",
              function_label_index);

  /* store multiple registers 14,15,0,...12 at 12 bytes from sp */
  fprintf (f, "\tSTM\tr14,r12,12(sp)\n");

  /* r3 == saved callee stack pointer */
  fprintf (f, "\tLR\tr3,sp\n");

  /* 4(r15) == stackframe size */
  fprintf (f, "\tSL\tsp,4(,r15)\n");

  /* r11 points to arg list in callers stackframe; was passed in r2 */
  fprintf (f, "\tLR\tr11,r2\n");

  /* store callee stack pointer at 8(sp) */
  /* fprintf (f, "\tST\tsp,8(,r3)\n ");  wasted cycles, no one uses this ... */

  /* backchain -- store caller sp at 4(callee_sp)  */
  fprintf (f, "\tST\tr3,4(,sp)\n ");

  fprintf (f, "\t.drop\tr15\n");
  /* Place contents of the PSW into r3
     that is, place the address of "." into r3 */
  fprintf (f, "\tBASR\tr%d,0\n", BASE_REGISTER);
  fprintf (f, "\t.using\t.,r%d\n", BASE_REGISTER);
  function_first = 1;
  function_label_index ++;

  fprintf (f, ".LPG%d:\n", mvs_page_num  );
  fprintf (f, "\tL\tr%d,=A(.LPGT%d)\n", PAGE_REGISTER, mvs_page_num);
  fprintf (f, "# Function code\n");

  mvs_free_label_list ();
  mvs_page_code = 6;
  mvs_page_lit = 4;
  mvs_check_page (f, 0, 0);
  function_base_page = mvs_page_num;

  /* find all labels in this routine */
  i370_label_scan ();
}
#endif /* TARGET_ELF_ABI */
