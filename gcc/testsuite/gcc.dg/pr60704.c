/* { dg-do compile } */
/* { dg-options "-flive-range-shrinkage" } */
/* { dg-additional-options "-march=amdfam10" { target { i?86-*-* x86_64-*-* } } } */

struct S
{
  int n;
};

int
foo (struct S s, double a)
{
  return s.n * a;
}
