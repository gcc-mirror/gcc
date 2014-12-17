/* PR middle-end/63568 */
/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int
fn1 (int a, int b, int m)
{
  return (a & ~m) | (b & m);
}

int
fn2 (int a, int b, int m)
{
  return (a & ~m) | (m & b);
}

int
fn3 (int a, int b, int m)
{
  return (~m & a) | (m & b);
}

int
fn4 (int a, int b, int m)
{
  return (~m & a) | (b & m);
}

int
fn5 (int a, int b, int m)
{
  return (b & m) | (a & ~m);
}

int
fn6 (int a, int b, int m)
{
  return (m & b) | (a & ~m);
}

int
fn7 (int a, int b, int m)
{
  return (m & b) | (~m & a);
}

int
fn8 (int a, int b, int m)
{
  return (b & m) | (~m & a);
}

/* { dg-final { scan-tree-dump-not " \\| " "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
