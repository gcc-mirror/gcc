/* PR debug/44028 */
/* { dg-do compile } */
/* { dg-options "-O3 -fcompare-debug" } */
/* { dg-options "-O3 -fsched-pressure -fschedule-insns -fcompare-debug" { target i?86-*-* x86_64-*-* } } */

struct S { int val[16]; };

static inline int
bar (struct S x)
{
  long double pc = 0;
  int i;
  for (i = 0; i < 16; i++)
    pc += x.val[i];
  return pc;
}

int
foo (struct S x)
{
  return bar (x);
}
