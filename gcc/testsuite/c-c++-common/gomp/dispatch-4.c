/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

void f2 (int a);

void test (void)
{
  int a;

#pragma omp dispatch device(-25373654)
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(-25373654\\);" 1 "gimple" } } */
  f2 (a);
#pragma omp dispatch device(a + a)
/* { dg-final { scan-tree-dump-times "(D\.\[0-9]+) = a \\* 2;.*#pragma omp dispatch.*__builtin_omp_set_default_device \\(\\1\\);.*f2 \\(a\\)" 2 "gimple" } } */
  f2 (a);
}

/* { dg-final { scan-tree-dump-times "(D\.\[0-9]+) = __builtin_omp_get_default_device \\(\\);.*__builtin_omp_set_default_device \\(\\1\\);" 4 "gimple" } } */
