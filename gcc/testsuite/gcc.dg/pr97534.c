/* PR target/97534 - ICE in decompose on arm*-*-*.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -O2 -g" } */

int f (int a)
{
  int b;
  __atomic_fetch_sub(&b, (int)(-__INT_MAX__ - 1), (int)0);
}
