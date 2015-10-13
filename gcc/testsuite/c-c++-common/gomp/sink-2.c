/* { dg-do compile } */

void bar (int *);

void
foo ()
{
  int i,j;
#pragma omp parallel for ordered(1)
  for (i=0; i < 100; ++i)
    {
#pragma omp ordered depend(sink:i-1)
    bar(&i);
#pragma omp ordered depend(source)
    }
}
