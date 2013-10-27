/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

int f (long long a, long long b)
{
  return (a * b) >> 16;
}
