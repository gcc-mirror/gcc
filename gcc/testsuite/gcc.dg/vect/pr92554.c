/* { dg-do compile } */

short int w9;

void __attribute__ ((simd))
zc (int in)
{
  int va = 1;

  w9 *= va != 0 ? in < 0 : 0;
}
