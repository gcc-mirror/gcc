/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-switchconv" } */

typedef enum { a = 5, b = 6, c = 7, d = 8, e = 9 } X;

int h1 (X x)
{
  switch (x) {
  case a:
  case b:
  case c:
  case d:
  case e:
    return 1;
  default:
    return 0;
    }
}

/* { dg-final { scan-tree-dump-times "CSWTCH" 0 "switchconv" } } */
/* { dg-final { cleanup-tree-dump "switchconv" } } */
