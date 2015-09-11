
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1-details" } */

extern void bar (char, char);
int
foo (char b)
{
  char a;
  a = b;
  b = 'b';
  bar (a, b);
  b = a;
  if (b == 0)
    a++;
  return a + b;
}

/* { dg-final { scan-tree-dump-times "factor conversion out" 0 "phiopt1" } } */
