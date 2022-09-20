/* { dg-options "-O2" } */

#pragma GCC target "+nosimd+fp"

void
foo (__Float32x4_t *ptr)
{
  ptr[0] = ptr[2];
  ptr[1] = ptr[3];
}

/* { dg-final { scan-assembler {\tldp\tq[0-9]+, q[0-9]+} } } */
/* { dg-final { scan-assembler {\tstp\tq[0-9]+, q[0-9]+} } } */
