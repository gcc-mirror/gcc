// PR c++/105839
// { dg-do compile { target c++11 } }
// { dg-options "-fopenmp" }

template <typename T>
void
foo (const T &x)
{
  [&] (auto &&y)		// { dg-error "use of 'auto' in lambda parameter declaration only available with" "" { target c++11_only } }
  {
    #pragma omp parallel for
    for (auto &&[v1, v2] : x)	// { dg-error "cannot decompose non-array non-class type 'const int'" }
      ;				// { dg-error "invalid type for iteration variable" "" { target c++14 } .-1 }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-2 }
  } ([]{});			// { dg-error "no match for call to" "" { target c++11_only } }
				// { dg-error "invalid user-defined conversion from" "" { target c++11_only } .-1 }
				// { dg-error "invalid conversion from" "" { target c++11_only } .-2 }
}

void
bar ()
{
  int a[10];
  foo (a);
}
