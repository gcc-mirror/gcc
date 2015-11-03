/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

/* Test that we adjust pointer offsets for sink variables
   correctly.  */

typedef struct {
    char stuff[400];
} foo;

foo *end, *begin, *p;

template<int N>
void
funk ()
{
  int i,j;
#pragma omp parallel for ordered(1)
  for (p=end; p > begin; p--)
    {
#pragma omp ordered depend(sink:p+1)
      void bar ();
        bar();
#pragma omp ordered depend(source)
    }
}

void foobar()
{
  funk<3>();
}

/* { dg-final { scan-tree-dump-times "depend\\(sink:p\\+400\\)" 1 "gimple" } } */
