/* { dg-do compile } */
/* { dg-options "-O" } */

int foo(int i)
{
  return (1LL >> 128 * i) && i;
}
