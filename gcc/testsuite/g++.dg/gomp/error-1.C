// { dg-do compile { target c++17 } }

void
foo ()
{
  if constexpr (false)
    {
      #pragma omp error								// { dg-bogus "'pragma omp error' encountered" }
    }
  else
    {
      #pragma omp error at(compilation) severity(warning) message("foo")	// { dg-warning "'pragma omp error' encountered: foo" }
    }
  if constexpr (true)
    {
      #pragma omp error message("bar")						// { dg-error "'pragma omp error' encountered: bar" }
    }
  else
    {
      #pragma omp error message("baz")						// { dg-bogus "'pragma omp error' encountered" }
    }
}

template <typename T>
bool
bar (T x)
{
  #pragma omp error at(execution) message (x)
  return false;
}

bool a = bar ("foo");

template <typename T>
bool
baz (T x)
{
  #pragma omp error at(execution) message (x)					// { dg-error "could not convert" }
  return false;
}

bool b = baz (L"foo");
