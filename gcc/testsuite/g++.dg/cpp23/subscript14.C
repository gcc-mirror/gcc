// PR c++/109319
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { static int &operator[] (int x) { static int a[2]; return a[x]; } };	// { dg-warning "may be a static member function only with" "" { target c++20_down } }
struct B { int &operator[] (int x) { static int b[2]; return b[x]; } };
int c[2];

template <typename T, typename U, typename V>
int
foo ()
{
  A a;
  ++a[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
  B b;			// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
  ++b[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
			// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
  ++c[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
  T d;			// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
  ++d[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
  U e;			// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
  ++e[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
  extern V f[2];	// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
  ++f[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
  return 0;		// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
}

int f[2];

int
main ()
{
  A a;
  ++a[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
  B b;			// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
  ++b[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
			// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
  ++c[0, 1];		// { dg-warning "top-level comma expression in array subscript changed meaning" "" { target c++23 } }
  foo <A, B, int> ();	// { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_only } .-1 }
  if (a.operator[] (1) != 3 || b.operator[] (1) != 3 || c[1] != 2 || f[1] != 1)
    __builtin_abort ();
}
