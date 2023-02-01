// PR c++/108607
// { dg-do compile { target c++14 } }
// { dg-options "-fopenmp" }

constexpr int
bar (int x)
{
  return x;
}

constexpr int
foo (int x)			// { dg-message "declared here" "" { target c++20_down } }
{				// { dg-message "is not usable as a 'constexpr' function because" "" { target c++23 } .-1 }
  #pragma omp scope		// { dg-warning "is not a constant expression" "" { target c++20_down } }
  x = bar (x);			// { dg-error "is not a constant expression" "" { target c++23 } .-1 }
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
      #pragma omp scope		// { dg-error "statement is not a constant expression" }
      x = bar (x);
      return x;
    case 3:
      #pragma omp parallel	// { dg-error "statement is not a constant expression" }
      x = bar (x);
      return x;
    case 4:
      #pragma omp task		// { dg-error "statement is not a constant expression" }
      x = bar (x);
      return x;
    default:
      return -1;
    }
}

constexpr int a = foo (1);	// { dg-error "called in a constant expression" }
constexpr int b = baz (42);
constexpr int c = baz (2);
constexpr int d = baz (3);
constexpr int e = baz (4);
