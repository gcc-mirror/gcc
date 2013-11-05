/* { dg-do compile } */
/* { dg-options "-O3 -fipa-pta" } */

void f(int p, short q)
{
  f(0, 0);
}
