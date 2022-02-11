/* { dg-do compile } */
/* { dg-options "-O2" } */

#define N 8
extern int src[N], dst[N];

void
foo (void)
{
  __builtin_memcpy (dst, src, N * sizeof (int));
}

/* { dg-final { scan-assembler {ldp\tq[0-9]*|ld1\t{v[0-9]*\.16b - v[0-9]*\.16b}} } } */
/* { dg-final { scan-assembler-not {ldp\tx[0-9]*} } } */
/* { dg-final { scan-assembler {stp\tq[0-9]*|st1\t{v[0-9]*\.16b - v[0-9]*\.16b}} } } */
/* { dg-final { scan-assembler-not {stp\tx[0-9]*} } } */
