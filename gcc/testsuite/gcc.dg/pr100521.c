/* { dg-do compile } */
/* { dg-options "-O2" } */

int
__builtin_clz (int a)
{
  return __builtin_clz(a);
}
