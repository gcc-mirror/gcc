/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-omplower" } */

/* Test that we adjust pointer offsets for sink variables
   correctly.  */

typedef struct {
    char stuff[400];
} foo;

void
funk (foo *begin, foo *end)
{
  foo *p;
#pragma omp parallel for ordered(1)
  for (p=end; p > begin; p--)
    {
#pragma omp ordered depend(sink:p+1)
      void bar ();
        bar();
#pragma omp ordered depend(source)
    }
}

/* { dg-final { scan-tree-dump-times "depend\\(sink:p\\+400\\)" 1 "omplower" } } */
