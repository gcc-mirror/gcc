/* { dg-do compile } */
/* { dg-options "-Oz -mbmi" } */
int m;

int foo(int x, int y)
{
  return (x & ~y) != 0;
}

int bar(int x)
{
  return (~x & m) != 0;
}
/* { dg-final { scan-assembler-not "andn\[ \\t\]+" } } */

