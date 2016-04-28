/* PR middle-end/70843 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (int x, int y)
{
  return ({ int a = 5; a += x; a *= y; a; }) ? : 2;
}
