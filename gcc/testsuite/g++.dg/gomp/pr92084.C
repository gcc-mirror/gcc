// PR c++/92084

void bar (int *, int);
int baz (int);

void
foo (int *x, int y)
{
#pragma omp taskgroup task_reduction (*: x[baz (y)])
  bar (x, baz (y));
}
