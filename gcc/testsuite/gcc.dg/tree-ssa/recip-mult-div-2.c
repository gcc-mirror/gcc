/* { dg-do compile } */
/* { dg-options "-O2 -freciprocal-math -fdump-tree-optimized" } */

/* (A / B) * (C / D) -> (A * C) / (B * D) forms two products that can leave
   the range of the type.  With B and D both large B * D is an infinity and
   the quotient becomes inf / inf; with both small it is a zero and the
   quotient becomes 0 / 0.  Either turns a finite result into a NaN, so
   -freciprocal-math on its own must not enable the rule.  */

double f (double a, double b, double c, double d)
{
  return (a / b) * (c / d);
}

/* { dg-final { scan-tree-dump-times " / " 2 "optimized" } } */
