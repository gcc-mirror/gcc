/* { dg-additional-options "-fdump-tree-omplower" } */

/* PR fortran/108545 */

/* { dg-final { scan-tree-dump "#pragma omp target enter data map\\(struct:my_struct \\\[len: 1\\\]\\) map\\(to:my_struct.u \\\[len: \[0-9\]+\\\]\\)" "omplower" } } */
/* { dg-final { scan-tree-dump "#pragma omp target enter data map\\(to:my_struct3 \\\[len: \[0-9\]+\\\]\\)" "omplower" } } */


volatile struct t {
  struct t2 { int *a; int c; } u;
  int b;
} my_struct;
volatile struct t3 { int *a; int c; } my_struct3;

void f()
{
  #pragma omp target enter data map(to:my_struct.u) map(to:my_struct.u.a)
  #pragma omp target enter data map(to:my_struct3) map(to:my_struct3.a)
}
