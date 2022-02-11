// PR c++/91391 - bogus -Wcomma-subscript warning.
// { dg-do compile { target c++20 } }

template<typename T, typename U>
int foo(T t, U u) { return t + u; }

void
fn (int *a, int b, int c)
{
  a[foo<int, int>(1, 2)];
  a[foo<int, int>(1, 2), foo<int, int>(3, 4)]; // { dg-warning "24:top-level comma expression in array subscript is deprecated" "" { target c++20_down } }
					       // { dg-error "top-level comma expression in array subscript changed meaning in" "" { target c++23 } .-1 }

  a[b < c, b < c]; // { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_down } }
		   // { dg-error "top-level comma expression in array subscript changed meaning in" "" { target c++23 } .-1 }
  a[b < c, b > c]; // { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_down } }
		   // { dg-error "top-level comma expression in array subscript changed meaning in" "" { target c++23 } .-1 }
  a[b > c, b > c]; // { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_down } }
		   // { dg-error "top-level comma expression in array subscript changed meaning in" "" { target c++23 } .-1 }
  a[b > c, b < c]; // { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_down } }
		   // { dg-error "top-level comma expression in array subscript changed meaning in" "" { target c++23 } .-1 }
  a[(b < c, b < c)];
  a[(b < c, b > c)];
  a[b << c, b << c]; // { dg-warning "top-level comma expression in array subscript is deprecated" "" { target c++20_down } }
		     // { dg-error "top-level comma expression in array subscript changed meaning in" "" { target c++23 } .-1 }
  a[(b << c, b << c)]; 
}
