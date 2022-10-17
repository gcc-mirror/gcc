void
foo (int *p)
{
  #pragma omp taskwait depend(iterator(i = 0:16) , in : p[i]) nowait depend(out : p[32])
}

void
bar (int *p)
{
  #pragma omp taskwait depend(mutexinoutset : p[0]) nowait	/* { dg-error "'mutexinoutset' kind in 'depend' clause on a 'taskwait' construct" } */
}

void
baz (void)
{
  #pragma omp taskwait nowait	/* { dg-error "'taskwait' construct with 'nowait' clause but no 'depend' clauses" } */
}
