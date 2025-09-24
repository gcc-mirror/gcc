/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx512f -mmove-max=128" } */

extern unsigned _BitInt(3719) a;
extern _BitInt(465) g;
void
foo(void)
{
  _BitInt(465) b = a >> 1860;
  g = b + b;
}
