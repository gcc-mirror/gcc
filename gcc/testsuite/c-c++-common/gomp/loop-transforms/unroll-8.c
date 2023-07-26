extern void dummy(int);

void
test1 ()
{
#pragma omp unroll full /* { dg-warning "Cannot apply full unrolling to infinite loop" } */
  for (int i = 101; i > 100; i++)
    dummy (i);
}


void
test2 ()
{
#pragma omp unroll full
  for (int i = 101; i != 100; i++)
    dummy (i);
}

void
test3 ()
{
#pragma omp unroll full /* { dg-warning "Cannot apply full unrolling to infinite loop" } */
  for (int i = 0; i <= 0; i--)
    dummy (i);
}

void
test4 ()
{
#pragma omp unroll full /* { dg-warning "Cannot apply full unrolling to infinite loop" } */
  for (int i = 101; i > 100; i=i+2)
    dummy (i);
}

void
test5 ()
{
#pragma omp unroll full /* { dg-warning "Cannot apply full unrolling to infinite loop" } */
  for (int i = -101; i < 100; i=i-10)
    dummy (i);
}

void
test6 ()
{
#pragma omp unroll full /* { dg-warning "Cannot apply full unrolling to infinite loop" } */
  for (int i = -101; i < 100; i=i-300)
    dummy (i);
}

void
test7 ()
{
#pragma omp unroll full /* { dg-warning "Cannot apply full unrolling to infinite loop" } */
  for (int i = 101; i > -100; i=i+300)
    dummy (i);

  /* Loop does not iterate, hence no warning. */
#pragma omp unroll full
  for (int i = 101; i > 101; i=i+300)
    dummy (i);
}

void
test8 ()
{
#pragma omp unroll full /* { dg-warning "Cannot apply full unrolling to infinite loop" } */
  for (int i = -21; i < -20; i=i-40)
    dummy (i);

  /* Loop does not iterate, hence no warning. */
#pragma omp unroll full
  for (int i = -21; i > 20; i=i-40)
    dummy (i);
}
