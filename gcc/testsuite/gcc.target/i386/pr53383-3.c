/* { dg-do compile } */
/* { dg-options "-O2 -mincoming-stack-boundary=3 -mpreferred-stack-boundary=3" } */

int
bar (int x)
{
  return x + 9;
}
