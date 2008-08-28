/* { dg-do compile } */
/* { dg-options "-O2 -msse4" } */

int testl (unsigned long *a, int b)
{
  return b ? 1 : __builtin_parityl (*a);
}
