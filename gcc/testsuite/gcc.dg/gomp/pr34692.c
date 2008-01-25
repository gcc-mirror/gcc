/* PR preprocessor/34692 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */
/* { dg-final { scan-tree-dump-times "#pragma omp parallel" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp for private" 1 "gimple" } } */

void
foo (void)
{
  int i;
#define FOO(y, x) y #x
#define BAR(x) x
#define BAZ(x) x
FOO (for (i = 0; i < 10; i++) { const char *vara =,
a
#define P parallel
#pragma omp P
#undef P
#define P for
b
#pragma omp P
#undef P
#define parallel atomic
cde f g h);
  }
}
