// PR c++/79664
// { dg-do compile }
// { dg-options "-std=c++14 -fopenmp -Winvalid-constexpr -pedantic-errors" }

constexpr int
f1 ()
{
  int i = 0;
#pragma omp parallel for			// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  for (i = 0; i < 10; ++i)
    ;
  return 0;
}

constexpr int
f2 ()
{
  int i = 0;
#pragma omp parallel				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 5;
  return 0;
}

constexpr int
f3 ()
{
  int i = 0;
#pragma omp task				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 5;
  return 0;
}

constexpr int
f4 ()
{
  int i = 0;
#pragma omp for					// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  for (i = 0; i < 10; ++i)
    ;
  return 0;
}

constexpr int
f5 ()
{
  int i = 0;
#pragma omp taskloop				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  for (i = 0; i < 10; ++i)
    ;
  return 0;
}

constexpr int
f6 ()
{
  int i = 0;
#pragma omp target teams			// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 5;
  return 0;
}

constexpr int
f7 ()
{
  int i = 0;
#pragma omp target data map(tofrom:i)		// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 5;
  return 0;
}

constexpr int
f8 ()
{
  int i = 0;
#pragma omp target				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 5;
  return 0;
}

constexpr int
f9 ()
{
  int i = 0;
#pragma omp sections				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  {
#pragma omp section				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
    i = 5;
  }
  return 0;
}

constexpr int
f10 ()
{
  int i = 0;
#pragma omp ordered				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 1;
  return 0;
}

constexpr int
f11 ()
{
  int i = 0;
#pragma omp critical				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 1;
  return 0;
}

constexpr int
f12 ()
{
  int i = 0;
#pragma omp single				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 1;
  return 0;
}

constexpr int
f13 ()
{
  int i = 0;
#pragma omp master				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 1;
  return 0;
}

constexpr int
f14 ()
{
  int i = 0;
#pragma omp taskgroup				// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 1;
  return 0;
}

constexpr int
f15 ()
{
  int i = 0;
#pragma omp target update to(i)			// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  i = 1;
  return 0;
}

constexpr int
f16 ()
{
  int i = 0;
#pragma omp target update to(i)			// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  return 0;
}

constexpr int
f17 ()
{
  int i = 0;
#pragma omp target enter data map(to:i)		// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  return 0;
}

constexpr int
f18 ()
{
  int i = 0;
#pragma omp target exit data map(from:i)	// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  return 0;
}
