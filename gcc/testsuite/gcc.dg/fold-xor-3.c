/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int
fn1 (signed int x, signed int y)
{
  signed int tem1 = x & y;
  signed int tem2 = x | y;
  return tem1 ^ tem2;
}

unsigned int
fn2 (unsigned int x, unsigned int y)
{
  unsigned int tem1 = x & y;
  unsigned int tem2 = x | y;
  return tem1 ^ tem2;
}

int
fn3 (signed int x, signed int y)
{
  signed int tem1 = x & y;
  signed int tem2 = x | y;
  return tem2 ^ tem1;
}

unsigned int
fn4 (unsigned int x, unsigned int y)
{
  unsigned int tem1 = x & y;
  unsigned int tem2 = x | y;
  return tem2 ^ tem1;
}

/* { dg-final { scan-tree-dump-not " & " "cddce1" } } */
/* { dg-final { scan-tree-dump-not " \\| " "cddce1" } } */
