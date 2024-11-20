/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-ompexp" } */

void f2 (void* p);

void test (void)
{
  void *p;

#pragma omp dispatch
/* { dg-final { scan-tree-dump-not "__builtin_GOMP_task " "ompexp" } } */
  f2 (p);
#pragma omp dispatch depend(inout: p)
/* { dg-final { scan-tree-dump-times "(D\.\[0-9]+)\\\[2] = &p;\[ \n]*__builtin_GOMP_taskwait_depend \\(&\\1\\);" 2 "ompexp" } } */
  f2 (p);
}


