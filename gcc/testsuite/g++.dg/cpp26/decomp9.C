// P0963R3 - Structured binding declaration as a condition
// { dg-do compile { target c++11 } }
// { dg-options "" }

int a[4];
struct S { int i, j; };
struct T { int i, j, k; explicit operator bool () const noexcept; } t;
enum E { E0, E1 };
struct U { int i, j, k, l; operator E () const noexcept; } u;
int w;
union X { int i; long j; } x;

void
foo (const S &&s)
{
  if (auto [ i, j, k, l ] = a)			// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;						// { dg-error "array initializer for structured binding declaration in condition" "" { target *-*-* } .-1 }
  if (auto & [a, b, c] = "ht")			// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;						// { dg-error "array initializer for structured binding declaration in condition" "" { target *-*-* } .-1 }
  if (auto const & [i, j] = s)			// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;						// { dg-error "could not convert '<structured bindings>' from 'const S' to 'bool'" "" { target *-*-* } .-1 }
  if (auto const & [i, j, k] = t)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    w = i + j + k;
  else
    w = i - j * k;
  if (auto [i, j, k, l] = u)			// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;
  if (auto [i] = x)				// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;						// { dg-error "cannot decompose union type 'X'" "" { target *-*-* } .-1 }
						// { dg-error "could not convert '<structured bindings>' from 'X' to 'bool'" "" { target *-*-* } .-2 }
  if (auto [i, j, k] = s)			// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;						// { dg-error "3 names provided for structured binding" "" { target *-*-* } .-1 }
						// { dg-error "could not convert '<structured bindings>' from 'S' to 'bool'" "" { target *-*-* } .-2 }
  if (auto [i] = s)				// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;						// { dg-error "only 1 name provided for structured binding" "" { target *-*-* } .-1 }
						// { dg-error "could not convert '<structured bindings>' from 'S' to 'bool'" "" { target *-*-* } .-2 }
  switch (auto [a, b, c] = "ht")		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {						// { dg-error "array initializer for structured binding declaration in condition" "" { target *-*-* } .-1 }
    default:					// { dg-error "switch quantity not an integer" "" { target *-*-* } .-2 }
      break;
    }
  switch (auto const & [i, j] = s)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {						// { dg-error "switch quantity not an integer" "" { target *-*-* } .-1 }
    case 1:
    default:
      break;
    }
  switch (auto const & [i, j, k] = t)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {						// { dg-error "switch quantity not an integer" "" { target *-*-* } .-1 }
    case 1:
    default:
      break;
    }
  switch (auto [i, j, k, l] = u)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
    case E0:
      ++i; ++j;
      break;
    default:
      ++k; ++l;
      break;
    }
  if (static auto [i, j, k] = t)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;						// { dg-error "'static' invalid in condition" "" { target *-*-* } .-1 }
						// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-2 }
  if (constexpr auto [i, j, k] = t)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;						// { dg-error "structured binding declaration cannot be 'constexpr'" "" { target *-*-* } .-1 }
}
