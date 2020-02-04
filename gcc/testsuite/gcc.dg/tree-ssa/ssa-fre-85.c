/* { dg-do compile } */
/* { dg-options "-O -fstrict-aliasing -fdump-tree-fre1-details" } */

struct X { int i; int j; };

struct X x, y;
void foo ()
{
  x.i = 1;
  y = x;
  y.i = 1; // redundant
}

/* { dg-final { scan-tree-dump "Deleted redundant store y.i" "fre1" } } */
