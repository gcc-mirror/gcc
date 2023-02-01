// PR c++/108607
// { dg-do compile { target c++14 } }
// { dg-options "-fopenmp" }

constexpr int
bar (int x)
{
  return x;
}

constexpr int
baz (int x)
{
  switch (x)
    {
    case 42:
      return 0;
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

constexpr int b = baz (42);
constexpr int d = baz (3);
constexpr int e = baz (4);
