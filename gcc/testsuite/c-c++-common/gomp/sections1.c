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
    bar (13);
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
    bar (17);
  }
  #pragma omp sections
  {
    bar (18);
    #pragma omp section
  }				/* { dg-error "expression before" } */
  #pragma omp sections
  {
    #pragma omp section
    #pragma omp section		/* { dg-error "may only be used in" } */
    bar (19);
  }
  #pragma omp sections
  {
    bar (20);
    #pragma omp section
    #pragma omp section		/* { dg-error "may only be used in" } */
    bar (21);
  }
  #pragma omp sections
  {
    bar (22);
    #pragma omp section
  }				/* { dg-error "expression before" } */
}
