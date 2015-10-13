// { dg-do compile }

struct S;	// { dg-message "forward declaration" }
void foo (S &);

void
f1 (S &x)	// { dg-error "has incomplete type" }
{
#pragma omp parallel private (x)
  foo (x);
}

void
f2 (S &x)	// { dg-error "has incomplete type" }
{
#pragma omp parallel firstprivate (x)
  foo (x);
}

void
f3 (S &x)	// { dg-error "has incomplete type" }
{
#pragma omp parallel for lastprivate (x)
  for (int i = 0; i < 10; i++)
    foo (x);
}
