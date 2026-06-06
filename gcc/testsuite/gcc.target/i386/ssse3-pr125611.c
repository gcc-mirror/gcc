/* PR target/125611 */
/* { dg-do compile } */
/* { dg-options "-O2 -mssse3 -mno-sse4.1" } */

typedef __attribute__((__vector_size__(16))) short V;
struct __attribute__((__packed__)) { V v[4]; } *u;

void
foo (void)
{
  u->v[2] &= __builtin_ia32_pmulhrsw128 (u->v[0], u->v[1]);
}
