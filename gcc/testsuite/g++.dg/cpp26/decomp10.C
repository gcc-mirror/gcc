// P0963R3 - Structured binding declaration as a condition
// { dg-do compile { target c++11 } }
// { dg-options "" }

_Complex int c;
int __attribute__((__vector_size__ (4 * sizeof (int)))) v;

void
foo ()
{
  if (auto [i,j] = c)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;
  if (auto [i,j,k,l] = v)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    ;				// { dg-error "could not convert '<structured bindings>' from '\[^\n\r]*' to 'bool'" "" { target *-*-* } .-1 }
}
