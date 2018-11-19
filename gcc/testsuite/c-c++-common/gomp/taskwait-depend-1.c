void
foo (int *p)
{
  #pragma omp taskwait depend(iterator(i = 0:16) , in : p[i]) depend(out : p[32])
}

void
bar (int *p)
{
  #pragma omp taskwait depend(mutexinoutset : p[0])	/* { dg-error "'mutexinoutset' kind in 'depend' clause on a 'taskwait' construct" } */
}
