/* { dg-do compile } */

int i,j, N;

extern void bar();

void
funk ()
{
#pragma omp parallel for ordered(2)
  for (i=0; i < N; i += 3)
    for (j=0; j < N; ++j)
    {
#pragma omp ordered depend(sink:i-8,j-1) /* { dg-warning "refers to iteration never in the iteration space" } */
#pragma omp ordered depend(sink:i+3,j-1) /* { dg-warning "waiting for lexically later iteration" } */
        bar();
#pragma omp ordered depend(source)
    }
}
