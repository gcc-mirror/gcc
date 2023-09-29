/* { dg-do compile } */
/* { dg-options "-std=c++03 -march=rv64gcv -mabi=lp64d -Ofast -ftree-vectorize --param=riscv-autovec-preference=scalable" } */

struct a
{
  int b;
  int c;
};
int d;
a
e ()
{
  a f;
  int g = d - 1, h = d / 2 - 1;
  f.b = g;
  f.c = h;
  return f;
}
