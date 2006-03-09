/* { dg-do compile } */

extern void bar(int);

void foo (void)
{
  #pragma omp master
    bar(0);

  #pragma omp master
  {
    bar(1);
    bar(2);
  }

  /* Yes, this is legal -- structured-block contains statement contains
     openmp-construct contains master-construct.  */
  #pragma omp master
  #pragma omp master
  #pragma omp master
    ;
}
