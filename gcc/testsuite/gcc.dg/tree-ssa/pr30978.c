/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int foo(int a)
{
  unsigned int b = a > 0;
  char c = b;
  _Bool d = c == 0;
  int e = !d;
  return e;
}

/* { dg-final { scan-tree-dump-times " = " 2 "optimized" } } */
/* One comparison and one extension to int.  */
/* { dg-final { scan-tree-dump " = a_..D. > 0;" "optimized" } } */
/* { dg-final { scan-tree-dump "e_. = \\\(int\\\)" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
