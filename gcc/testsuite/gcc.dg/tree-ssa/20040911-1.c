/* Verify that points-to information is handled properly for PTR + OFFSET
   pointer arithmetics.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-alias1-vops" } */

char buf[4], *q;
int foo (int i)
{
  char c, *p;
  q = &c;
  p = buf;
  if (i)
    p = p + 3;
  else
    p = p + 2;
  *p = 6;
  c = 8;
  return *p;
}

/* { dg-final { scan-tree-dump-not "VUSE <c" "alias1" } } */
/* { dg-final { cleanup-tree-dump "alias1" } } */
