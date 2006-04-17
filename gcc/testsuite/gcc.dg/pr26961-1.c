/* { dg-do compile } */
/* { dg-options "-O2" } */

long long foo(int i, int j)
{
  return i ? (long long)(!j) : 0;
}

