/* { dg-do compile } */
/* { dg-options "-O2 -mpreferred-stack-boundary=3 -mincoming-stack-boundary=3" } */

int
bar (int x)
{
  return x + 9;
}
