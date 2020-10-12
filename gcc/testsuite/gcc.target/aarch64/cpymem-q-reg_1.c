/* { dg-do compile } */
/* { dg-options "-O2" } */

#define N 8
extern int src[N], dst[N];

void
foo (void)
{
  __builtin_memcpy (dst, src, N * sizeof (int));
}

/* { dg-final { scan-assembler {ldp\tq[0-9]*} } } */
/* { dg-final { scan-assembler-not {ldp\tx[0-9]*} } } */
/* { dg-final { scan-assembler {stp\tq[0-9]*} } } */
/* { dg-final { scan-assembler-not {stp\tx[0-9]*} } } */
