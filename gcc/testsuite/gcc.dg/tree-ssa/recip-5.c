/* { dg-options "-O1 -funsafe-math-optimizations -ftrapping-math -fdump-tree-recip -fdump-tree-optimized" } */
/* { dg-do compile } */

/* Test the reciprocal optimizations together with trapping math.  */

extern int f2();

double f1(double y, double z, double w, double j, double k)
{
  double b, c, d, e, f, g;

  if (f2 ())
    /* inserts one division here */
    b = 1 / y, c = z / y, d = j / y;
  else
    /* one division here */
    b = 3 / y, c = w / y, d = k / y;

  /* and one here, that should be removed afterwards but is not right now */
  e = b / y;
  f = c / y;
  g = d / y;

  return e + f + g;
}

/* { dg-final { scan-tree-dump-times " / " 3 "recip" } } */
/* { dg-final { scan-tree-dump-times " / " 2 "optimized" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "recip" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

