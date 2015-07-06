/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

int a, b;
float c;
int fn2();
void fn1()
{
  if (fn2() <= 1. - c)
    b = a;
}
