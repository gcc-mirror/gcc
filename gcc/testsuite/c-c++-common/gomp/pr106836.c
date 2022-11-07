/* PR c/106836 */

void
foo (void)
{
#pragma omp target parallel depend (source)	/* { dg-error "'depend\\\(source\\\)' is only allowed in 'omp ordered'" } */
  ;
#pragma omp taskwait
}
