// { dg-do compile }

struct S { int s; } s;

void
foo (void)
{
  #pragma omp parallel
  {
    #pragma omp cancel parallel if (s)	// { dg-error "could not convert 's' from 'S' to 'bool'" }
  }
}
