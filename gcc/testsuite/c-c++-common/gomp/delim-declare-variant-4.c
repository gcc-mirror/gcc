/* { dg-do compile } */

/* Check that a proper error is diagnosed if an "omp begin declare variant"
   construct has an invalid selector, and that this causes the whole variant
   to be skipped over rather than a duplicate definition error.  */

int foo (int a)
{
  return a;
}

#pragma omp begin declare variant match (construct=target)  /* { dg-error "expected '\{' before 'target'" } */
int foo (int a)
{
  return a + 1;
}

#pragma omp end declare variant

int bar (int x)
{
  return x;
}

#pragma omp begin declare variant match (gibberish = {blah(1)})  /* { dg-error "expected context selector set name" } */
int bar (int x)
{
  return x + 2;
}

#pragma omp end declare variant
