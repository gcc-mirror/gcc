/* { dg-do compile } */

void foo()
{
  int i;

  #pragma omp for nowait
  for (i = 0; i < 10; ++i) ;

  #pragma omp for nowait nowait		/* { dg-error "too many" } */
  for (i = 0; i < 10; ++i) ;

  #pragma omp for ordered
  for (i = 0; i < 10; ++i) ;

  #pragma omp for ordered ordered	/* { dg-error "too many" } */
  for (i = 0; i < 10; ++i) ;
}
