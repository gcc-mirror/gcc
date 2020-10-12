/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-reassoc" } */

#define max(a,b) (((a) > (b))? (a) : (b))

int foo(int x)
{
  int y = max(x,12345);
  return max(y,87654);
}

/* { dg-final { scan-assembler-not "12345" } } */
