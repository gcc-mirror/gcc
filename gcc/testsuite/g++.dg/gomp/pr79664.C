// PR c++/79664
// { dg-do compile }
// { dg-options "-std=c++14 -fopenmp" }

constexpr int
f1 ()
{
  int i = 0;
#pragma omp parallel for			// { dg-error "is not a constant expression" }
  for (i = 0; i < 10; ++i)
    ;
  return 0;
}

constexpr int
f2 ()
{
  int i = 0;
#pragma omp parallel				// { dg-error "is not a constant expression" }
  i = 5;
  return 0;
}

constexpr int
f3 ()
{
  int i = 0;
#pragma omp task				// { dg-error "is not a constant expression" }
  i = 5;
  return 0;
}

constexpr int
f4 ()
{
  int i = 0;
#pragma omp for					// { dg-error "is not a constant expression" }
  for (i = 0; i < 10; ++i)
    ;
  return 0;
}

constexpr int
f5 ()
{
  int i = 0;
#pragma omp taskloop				// { dg-error "is not a constant expression" }
  for (i = 0; i < 10; ++i)
    ;
  return 0;
}

constexpr int
f6 ()
{
  int i = 0;
#pragma omp target teams			// { dg-error "is not a constant expression" }
  i = 5;
  return 0;
}

constexpr int
f7 ()
{
  int i = 0;
#pragma omp target data map(tofrom:i)		// { dg-error "is not a constant expression" }
  i = 5;
  return 0;
}

constexpr int
f8 ()
{
  int i = 0;
#pragma omp target				// { dg-error "is not a constant expression" }
  i = 5;
  return 0;
}

constexpr int
f9 ()
{
  int i = 0;
#pragma omp sections				// { dg-error "is not a constant expression" }
  {
#pragma omp section
    i = 5;
  }
  return 0;
}

constexpr int
f10 ()
{
  int i = 0;
#pragma omp ordered				// { dg-error "is not a constant expression" }
  i = 1;
  return 0;
}

constexpr int
f11 ()
{
  int i = 0;
#pragma omp critical				// { dg-error "is not a constant expression" }
  i = 1;
  return 0;
}

constexpr int
f12 ()
{
  int i = 0;
#pragma omp single				// { dg-error "is not a constant expression" }
  i = 1;
  return 0;
}

constexpr int
f13 ()
{
  int i = 0;
#pragma omp master				// { dg-error "is not a constant expression" }
  i = 1;
  return 0;
}

constexpr int
f14 ()
{
  int i = 0;
#pragma omp taskgroup				// { dg-error "is not a constant expression" }
  i = 1;
  return 0;
}

constexpr int
f15 ()
{
  int i = 0;
#pragma omp target update to(i)			// { dg-error "is not a constant expression" }
  i = 1;
  return 0;
}

constexpr int
f16 ()
{
  int i = 0;
#pragma omp target update to(i)			// { dg-error "is not a constant expression" }
  return 0;
}

constexpr int
f17 ()
{
  int i = 0;
#pragma omp target enter data map(to:i)		// { dg-error "is not a constant expression" }
  return 0;
}

constexpr int
f18 ()
{
  int i = 0;
#pragma omp target exit data map(from:i)	// { dg-error "is not a constant expression" }
  return 0;
}
