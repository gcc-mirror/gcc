// PR c++/104623
// { dg-do compile }

void
foo ()
{
  struct __attribute__() a	// { dg-error "expected primary-expression before" }
  #pragma omp task
}
