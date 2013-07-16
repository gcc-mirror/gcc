/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mno-sse" { target { i?86-*-* x86_64-*-* } } } */

typedef long long v2tw __attribute__ ((vector_size (2 * sizeof (long long))));

void tiger_block_v2 (long long in1, v2tw *res)
{
  v2tw i1 = { in1, in1 };

  *res = i1 << 1;
}
