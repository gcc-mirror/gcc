/* PR rtl-optimization/116372 */
/* { dg-do run } */
/* { dg-options "-O1" } */ 
/* { dg-additional-options "-march=z13" { target s390x-*-* } } */

long x = -0x7fffffff - 1;
int main (void)
{
  long y = x % (-0xf - 1);
  if (-0x7fffffff - 1 + y == x == 0)
    __builtin_abort ();
  return 0;
}
