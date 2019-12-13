// Test parsing of #pragma omp declare variant
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdump-tree-gimple" }

constexpr int score (int x) { return x; }
void f0 ();
#pragma omp declare variant (f0) match (user={condition(score(1))})
void f1 ();
void f2 ();
#pragma omp declare variant (f2) match (user={condition(score(1):1)})
void f3 ();
void f4 ();
#pragma omp declare variant (f4) match (user={condition(score(1):score(1))})
void f5 ();
void f6 ();
#pragma omp declare variant (f6) match (user={condition(score(1)==0)})
void f7 ();

void
test ()
{
  f1 ();	// { dg-final { scan-tree-dump-times "f0 \\\(\\\);" 1 "gimple" } }
  f3 ();	// { dg-final { scan-tree-dump-times "f2 \\\(\\\);" 1 "gimple" } }
  f5 ();	// { dg-final { scan-tree-dump-times "f4 \\\(\\\);" 1 "gimple" } }
  f7 ();	// { dg-final { scan-tree-dump-times "f7 \\\(\\\);" 1 "gimple" } }
}
