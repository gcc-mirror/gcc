/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=signed-integer-overflow" } */
/* { dg-additional-options "-std=gnu11" { target c } } */
/* { dg-additional-options "-std=c++11" { target c++ } } */

volatile int w, z;

__attribute__ ((noinline, noclone)) int
foo (int x, int y)
{
  z++;
  return x << y;
}

int
main ()
{
  w = foo (0, -__INT_MAX__);
  return 0;
}

/* { dg-output "shift exponent -\[^\n\r]* is negative" } */
