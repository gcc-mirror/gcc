// PR c++/108607
// { dg-do compile { target c++14 } }
// { dg-options "-fopenmp" }

constexpr int
bar (int x)
{
  return x;
}

constexpr int
foo (int x)
{
  #pragma omp scope		// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
  x = bar (x);
  return x;
}

constexpr int
baz (int x)
{
  switch (x)
    {
    case 42:
      return 0;
    case 2:
      #pragma omp scope		// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
      x = bar (x);
      return x;
    case 3:
      #pragma omp parallel	// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
      x = bar (x);
      return x;
    case 4:
      #pragma omp task		// { dg-error "OpenMP directives may not appear in 'constexpr' functions" }
      x = bar (x);
      return x;
    default:
      return -1;
    }
}

constexpr int a = foo (1);
constexpr int b = baz (42);
constexpr int c = baz (2);
constexpr int d = baz (3);
constexpr int e = baz (4);
