/* { dg-options "-O2 -mno-80387" } */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */

float test (unsigned int x)
{
  return (float) x;
}
