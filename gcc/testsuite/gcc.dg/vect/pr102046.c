/* { dg-do compile } */
/* { dg-additional-options "-O3 -fvect-cost-model=dynamic" } */
/* { dg-additional-options "-march=btver2" { target x86_64-*-* i?86-*-* } } */

struct S
{
  unsigned a, b;
};

struct S g;

void
foo (struct S *o)
{
  struct S s = g;
  s.b *= 3;
  s.a -= s.a / 2;
  *o = s;
}
