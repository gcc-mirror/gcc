struct S { int i : 1; int j : 4; long long k : 25; };
void bar (struct S, int);
#pragma omp declare target to (bar)

void
foo (struct S a, struct S b, struct S c, struct S d)
{
  #pragma omp target map (a)
  bar (a, 0);
  #pragma omp target map (a) map (b.i)		/* { dg-error "bit-field .b.\(S::\|\)i. in .map. clause" } */
  bar (a, b.i);
  #pragma omp target map (a) map (b.j)		/* { dg-error "bit-field .b.\(S::\|\)j. in .map. clause" } */
  bar (a, b.j);
  #pragma omp target map (a) map (b.k)		/* { dg-error "bit-field .b.\(S::\|\)k. in .map. clause" } */
  bar (a, b.k);
  #pragma omp target data map (a) map (b.i)	/* { dg-error "bit-field .b.\(S::\|\)i. in .map. clause" } */
  {
    #pragma omp target enter data map (alloc: a) map (to: c.j)		/* { dg-error "bit-field .c.\(S::\|\)j. in .map. clause" } */
    #pragma omp target exit data map (release: a) map (from: d.k)	/* { dg-error "bit-field .d.\(S::\|\)k. in .map. clause" } */
  }
}
