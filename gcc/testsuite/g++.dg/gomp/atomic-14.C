// PR middle-end/45423
// { dg-do compile }
// { dg-options "-fopenmp" }

bool *baz (), atomicvar;

int
foo (void)
{
  #pragma omp atomic
    (*baz ())--;	// { dg-error "use of an operand of type .bool." }
  #pragma omp atomic
    --(*baz ());	// { dg-error "use of an operand of type .bool." }
  #pragma omp atomic
    atomicvar--;	// { dg-error "use of an operand of type .bool." }
  #pragma omp atomic
    --atomicvar;	// { dg-error "use of an operand of type .bool." }
  return 0;
}
