/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define HUGE __DBL_MAX__
#define INF (HUGE + HUGE)
#define NAN (INF - INF)

double foo() {
  double x = -NAN;
  double y = NAN;
  return x + y;
}

/* { dg-final { scan-tree-dump-not "return 0\.0" "optimized" } } */
