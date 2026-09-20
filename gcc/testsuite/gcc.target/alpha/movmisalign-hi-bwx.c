/* Verify that a misaligned 2-byte load on a BWX target is expanded inline
   rather than being copied a byte at a time through a stack slot.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mno-safe-partial -mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned short
load_hi (const unsigned char *p)
{
  unsigned short v;

  __builtin_memcpy (&v, p, sizeof v);
  return v;
}

/* { dg-final { scan-assembler-times "\\sldbu\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\sstb\\s" } } */
/* { dg-final { scan-assembler-not "\\slda\\s\\\$30," } } */
