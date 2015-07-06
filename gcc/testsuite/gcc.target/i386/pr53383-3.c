/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse -mincoming-stack-boundary=3 -mpreferred-stack-boundary=3" } */

int
bar (int x)
{
  return x + 9;
}
