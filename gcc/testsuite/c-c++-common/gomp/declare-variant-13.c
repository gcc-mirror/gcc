/* { dg-do compile { target vect_simd_clones } } */
/* { dg-additional-options "-fdump-tree-gimple" } */
/* { dg-additional-options "-mno-sse3" { target { i?86-*-* x86_64-*-* } } } */

int f01 (int);
int f02 (int);
int f03 (int);
int f04 (int);
#pragma omp declare variant (f01) match (device={isa("avx512f")}) /* 4 or 8 */
#pragma omp declare variant (f02) match (implementation={vendor(score(3):gnu)},device={kind(cpu)}) /* (1 or 2) + 3 */
#pragma omp declare variant (f03) match (user={condition(score(9):1)})
#pragma omp declare variant (f04) match (implementation={vendor(score(6):gnu)},device={kind(host)}) /* (1 or 2) + 6 */
int f05 (int);

#pragma omp declare simd
int
test1 (int x)
{
  /* 0 or 1 (the latter if in a declare simd clone) constructs in OpenMP context,
     isa has score 2^2 or 2^3.  We can't decide on whether avx512f will match or
     not, that also depends on whether it is a declare simd clone or not and which
     one, but the f03 variant has a higher score anyway.  */
  return f05 (x);	/* { dg-final { scan-tree-dump-times "f03 \\\(x" 1 "gimple" } } */
}
