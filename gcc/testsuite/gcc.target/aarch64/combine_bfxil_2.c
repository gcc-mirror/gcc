/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long long
combine_non_consecutive (unsigned long long a, unsigned long long b)
{
  return (a & 0xfffffff200f00000ll) | (b & 0x00001000ffffffffll);
}

void
foo4 (unsigned long long a, unsigned long long b, unsigned long long *c,
  unsigned long long *d) {
  /* { dg-final { scan-assembler-not "bfxil\\t" } } */
  *c = combine_non_consecutive (a, b);
  *d = combine_non_consecutive (b, a);
}
