/* { dg-options "-O1 -funsafe-math-optimizations -fno-trapping-math -fdump-tree-recip" } */
/* { dg-do compile } */

/* Test inserting in a block that does not contain a division.  */

extern int f2();

double f1(double y, double z, double w)
{
  double b, c, d, e, f;

  if (g ())
    b = 1 / y, c = z / y;
  else
    b = 3 / y, c = w / y;

  d = b / y;
  e = c / y;
  f = 1 / y;

  return d + e + f;
}

/* { dg-final { scan-tree-dump-times " / " 1 "recip" } } */
/* { dg-final { cleanup-tree-dump "recip" } } */

