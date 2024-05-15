/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

extern int foo_p (int);
extern int bar;

int f01 (int);
int f02 (int);
int f03 (int);
int f04 (int);
#pragma omp declare variant (f01) match (device={isa("avx512f")}) /* 4 */
#pragma omp declare variant (f02) match (implementation={vendor(score(3):gnu)},device={kind(cpu)}) /* 1 + 3 */
#pragma omp declare variant (f03) match (user={condition(score(9):foo_p (bar))})
#pragma omp declare variant (f04) match (implementation={vendor(score(6):gnu)},device={kind(host)}) /* 1 + 6 */
int f05 (int);


int
test1 (int x)
{
  return f05 (x);
}

/* { dg-final { scan-tree-dump "f03 \\\(x" "gimple" } } */
/* { dg-final { scan-tree-dump "f04 \\\(x" "gimple" } } */
/* { dg-final { scan-tree-dump-not "f05 \\\(x" "gimple" } } */
