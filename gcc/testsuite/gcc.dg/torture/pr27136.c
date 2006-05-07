/* { dg-do compile } */
/* { dg-options "-ffast-math" } */

void foo()
{
  double x;

  for (x = 2; x < 10; x *= x)
    ;
}
