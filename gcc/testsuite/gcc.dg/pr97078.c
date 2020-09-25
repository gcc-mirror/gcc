/* { dg-do compile } */
/* { dg-options "-O2 -ffloat-store" } */

extern void foo (long double);

void bar (long double d)
{
  foo (d);
}
