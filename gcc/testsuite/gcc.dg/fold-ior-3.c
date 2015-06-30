/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int
fn1 (_Bool a)
{
  return ((int) a) | ((int) ~a);
}

int
fn2 (unsigned char a)
{
  return ((int) a) | ((int) ~a);
}

int
fn3 (unsigned short a)
{
  return ((int) a) | ((int) ~a);
}

int
fn4 (signed char a)
{
  return ((int) a) | ((int) ~a);
}

int
fn5 (signed short a)
{
  return ((int) a) | ((int) ~a);
}

/* { dg-final { scan-tree-dump-not "~" "cddce1" } } */
/* { dg-final { scan-tree-dump-not " \\| " "cddce1" } } */
