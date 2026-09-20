/* Verify that a misaligned 4-byte load and store are expanded inline with
   the ldq_u/extXl/extXh and ldq_u/mskXl/insXl/stq_u sequences rather than
   being copied a byte at a time through a stack slot.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mno-safe-partial" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned int
load_si (const unsigned char *p)
{
  unsigned int v;

  __builtin_memcpy (&v, p, sizeof v);
  return v;
}

void
store_si (unsigned char *p, unsigned int v)
{
  __builtin_memcpy (p, &v, sizeof v);
}

/* { dg-final { scan-assembler-times "\\sldq_u\\s" 4 } } */
/* { dg-final { scan-assembler-times "\\sextll\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sextlh\\s" 1 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\sldbu\\s" } } */
/* { dg-final { scan-assembler-not "\\sstb\\s" } } */
/* { dg-final { scan-assembler-not "\\slda\\s\\\$30," } } */
