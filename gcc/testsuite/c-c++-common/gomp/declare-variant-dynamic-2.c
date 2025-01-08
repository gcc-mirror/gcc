/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

extern int foo_p (int);
extern int bar;
extern int omp_get_default_device (void);

int f01 (int);
int f02 (int);
int f03 (int);
int f04 (int);
#pragma omp declare variant (f01) match (target_device={device_num(omp_get_default_device()), isa("avx512f")}) /* 4 */
#pragma omp declare variant (f02) match (user={condition(score(6):0)})
#pragma omp declare variant (f03) match (user={condition(score(5):foo_p (bar))})
#pragma omp declare variant (f04) match (user={condition(score(3):0)})
int f05 (int);

int
test1 (int x)
{
  return f05 (x);
}

/* f01 and f03 are the dynamic selectors, the fall-through is f05.
   f02 and f04 are static selectors and do not match.  */
/* { dg-final { scan-tree-dump "f01 \\\(x" "gimple" } } */
/* { dg-final { scan-tree-dump "f03 \\\(x" "gimple" } } */
/* { dg-final { scan-tree-dump "f05 \\\(x" "gimple" } } */
/* { dg-final { scan-tree-dump-not "f02 \\\(x" "gimple" } } */
/* { dg-final { scan-tree-dump-not "f04 \\\(x" "gimple" } } */
