/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int
fn1 (int x)
{
  return ~x | x;
}

int
fn2 (int x)
{
  return x | ~x;
}

unsigned int
fn3 (unsigned int x)
{
  return ~x | x;
}

unsigned int
fn4 (unsigned int x)
{
  return x | ~x;
}

int
fn5 (int x)
{
  return ~x | (unsigned) x;
}

int
fn6 (int x)
{
  return (unsigned) ~x | x;
}

int
fn7 (int x)
{
  return ~(unsigned) x | x;
}

/* { dg-final { scan-tree-dump-not "~" "cddce1" } } */
/* { dg-final { scan-tree-dump-not " \\| " "cddce1" } } */
