/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

typedef struct {
  int a, b, c, d;
} S;

#pragma omp declare mapper (S s) map(alloc: s.a) map(to: s.b) map(from: s.c) \
				 map(tofrom: s.d)
#pragma omp declare mapper (update: S s) map(s.a, s.b, s.c, s.d)

int main()
{
  S v;
#pragma omp target update to(v)
/* { dg-warning {dropping .from. clause during mapper expansion in .#pragma omp target update.} "" { target *-*-* } .-1 } */
/* { dg-warning {dropping .alloc. clause during mapper expansion in .#pragma omp target update.} "" { target *-*-* } .-2 } */
/* { dg-final { scan-tree-dump-times {(?n)update to\(v\.d\) to\(v\.b\)$} 1 "original" } } */
#pragma omp target update from(v)
/* { dg-warning {dropping .to. clause during mapper expansion in .#pragma omp target update.} "" { target *-*-* } .-1 } */
/* { dg-warning {dropping .alloc. clause during mapper expansion in .#pragma omp target update.} "" { target *-*-* } .-2 } */
/* { dg-final { scan-tree-dump-times {(?n)update from\(v\.d\) from\(v\.c\)$} 1 "original" } } */

#pragma omp target update to(mapper(update): v)
/* { dg-final { scan-tree-dump-times {(?n)update to\(v\.d\) to\(v\.c\) to\(v\.b\) to\(v\.a\)$} 1 "original" } } */
#pragma omp target update from(mapper(update): v)
/* { dg-final { scan-tree-dump-times {(?n)update from\(v\.d\) from\(v\.c\) from\(v\.b\) from\(v\.a\)$} 1 "original" } } */

#pragma omp target update to(present, mapper(update): v)
/* { dg-final { scan-tree-dump-times {(?n)update to\(present:v\.d\) to\(present:v\.c\) to\(present:v\.b\) to\(present:v\.a\)$} 2 "original" } } */
#pragma omp target update from(present, mapper(update): v)
/* { dg-final { scan-tree-dump-times {(?n)update from\(present:v\.d\) from\(present:v\.c\) from\(present:v\.b\) from\(present:v\.a\)$} 2 "original" } } */

#pragma omp target update to(present: v.a, v.b, v.c, v.d)
#pragma omp target update from(present: v.a, v.b, v.c, v.d)

  return 0;
}
