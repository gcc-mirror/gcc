/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void bar (int);

void
foo ()
{
  #pragma omp sections
  {
    bar (1);
    #pragma omp section
    {
      bar (2);
    }
  }
  #pragma omp sections
  {
    #pragma omp section
    bar (3);
    #pragma omp section
    {
      bar (4);
      bar (5);
    }
  }
  #pragma omp sections
  {
    {
      bar (6);
      bar (7);
    }
    #pragma omp section
    bar (8);
  }
  #pragma omp sections
  {
    #pragma omp section
    {
      bar (9);
    }
    #pragma omp section
    bar (10);
    #pragma omp section
    bar (11);
  }
  #pragma omp sections
  {
  }				/* { dg-error "expression before" } */
  #pragma omp sections
  {
    bar (12);
    bar (13);			/* { dg-error "pragma omp section" } */
    #pragma omp section
    bar (14);
  }
  #pragma omp sections
  {
    #pragma omp section
  }				/* { dg-error "expression before" } */
  #pragma omp sections
  {
    bar (15);
    #pragma omp section
    bar (16);
    bar (17);			/* { dg-error "pragma omp section" } */
  }
  #pragma omp sections
  {
    bar (18);
    #pragma omp section
  }				/* { dg-error "expression before" } */
}
