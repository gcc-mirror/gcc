/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

struct S { int s; };
void foo (char *);
void bar (int, char *, struct S, int *);
#pragma omp declare target to (bar)
#define N 16

void
f1 (int sc1, struct S ag1, int *pt1)
{
  char ar1[N];
  foo (ar1);
  #pragma omp target
  bar (sc1, ar1, ag1, pt1);
/* { dg-final { scan-tree-dump "firstprivate\\(sc1\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(tofrom:ar1" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(tofrom:ag1" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(firstprivate:pt1 .pointer assign" "gimple" } } */
}

void
f2 (int sc2, struct S ag2, int *pt2)
{
  char ar2[N];
  foo (ar2);
  #pragma omp target firstprivate (sc2, ar2, ag2, pt2) defaultmap (none)
  bar (sc2, ar2, ag2, pt2);
/* { dg-final { scan-tree-dump "firstprivate\\(sc2\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "firstprivate\\(ar2\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "firstprivate\\(ag2\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "firstprivate\\(pt2\\)" "gimple" } } */
}

void
f3 (int sc3, struct S ag3, int *pt3)
{
  char ar3[N];
  foo (ar3);
  #pragma omp target defaultmap(none:scalar) defaultmap(none:aggregate) \
		     map (sc3, ar3, ag3, pt3) defaultmap(none:pointer)
  bar (sc3, ar3, ag3, pt3);
/* { dg-final { scan-tree-dump "map\\(tofrom:sc3" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(tofrom:ar3" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(tofrom:ag3" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(tofrom:pt3" "gimple" } } */
}

void
f4 (int sc4, struct S ag4, int *pt4)
{
  char ar4[N];
  foo (ar4);
  #pragma omp target defaultmap(tofrom:scalar)
  bar (sc4, ar4, ag4, pt4);
/* { dg-final { scan-tree-dump "map\\(tofrom:sc4" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(tofrom:ar4" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(tofrom:ag4" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(firstprivate:pt4 .pointer assign" "gimple" } } */
}

void
f5 (int sc5, struct S ag5, int *pt5)
{
  char ar5[N];
  foo (ar5);
  #pragma omp target defaultmap(to)
  bar (sc5, ar5, ag5, pt5);
/* { dg-final { scan-tree-dump "map\\(to:sc5" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(to:ar5" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(to:ag5" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(to:pt5" "gimple" } } */
}

void
f6 (int sc6, struct S ag6, int *pt6)
{
  char ar6[N];
  foo (ar6);
  #pragma omp target defaultmap(firstprivate)
  bar (sc6, ar6, ag6, pt6);
/* { dg-final { scan-tree-dump "firstprivate\\(sc6\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "firstprivate\\(ar6\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "firstprivate\\(ag6\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "firstprivate\\(pt6\\)" "gimple" } } */
}

void
f7 (int sc7, struct S ag7, int *pt7)
{
  char ar7[N];
  foo (ar7);
  #pragma omp target defaultmap(alloc: scalar) defaultmap(from: aggregate) defaultmap(default: pointer)
  {
    int *q = &sc7;
    *q = 6;
    ag7.s = 5;
    int i;
    for (i = 0; i < N; ++i)
      ar7[i] = 7;
    bar (sc7, ar7, ag7, pt7);
  }
/* { dg-final { scan-tree-dump "map\\(alloc:sc7" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(from:ar7" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(from:ag7" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(firstprivate:pt7 .pointer assign" "gimple" } } */
}

void
f8 (int sc8, struct S ag8, int *pt8)
{
  char ar8[N];
  foo (ar8);
  #pragma omp target defaultmap(firstprivate:aggregate) defaultmap(none:scalar) \
		     defaultmap(tofrom:pointer) map(to: sc8)
  bar (sc8, ar8, ag8, pt8);
/* { dg-final { scan-tree-dump "map\\(to:sc8" "gimple" } } */
/* { dg-final { scan-tree-dump "firstprivate\\(ar8\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "firstprivate\\(ag8\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "map\\(tofrom:pt8" "gimple" } } */
}

void
f9 (int sc9, struct S ag9)
{
  char ar9[sc9 + 2];
  foo (ar9);
  #pragma omp target defaultmap(none) map(to: ar9, ag9) firstprivate (sc9)
  bar (sc9, ar9, ag9, &sc9);
}
