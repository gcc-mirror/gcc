// { dg-do compile { target c++17 } }

struct A { bool a, b; };
struct B { int a, b; };

void
foo ()
{
  auto [ a, b ] = A ();
  for (; auto [ a, b ] = A (); )			// { dg-error "structured bindings in conditions only available with" "" { target c++23_down } }
    ;							// { dg-error "could not convert '<structured bindings>' from 'A' to 'bool'" "" { target *-*-* } .-1 }
  for (; false; auto [ a, b ] = A ())			// { dg-error "expected" }
    ;
  if (auto [ a, b ] = A ())				// { dg-error "structured bindings in conditions only available with" "" { target c++23_down } }
    ;							// { dg-error "could not convert '<structured bindings>' from 'A' to 'bool'" "" { target *-*-* } .-1 }
  if (auto [ a, b ] = A (); auto [ c, d ] = A ())	// { dg-error "structured bindings in conditions only available with" "" { target c++23_down } }
    ;							// { dg-error "could not convert '<structured bindings>' from 'A' to 'bool'" "" { target *-*-* } .-1 }
  if (int d = 5; auto [ a, b ] = A ())			// { dg-error "structured bindings in conditions only available with" "" { target c++23_down } }
    ;							// { dg-error "could not convert '<structured bindings>' from 'A' to 'bool'" "" { target *-*-* } .-1 }
  switch (auto [ a, b ] = B ())				// { dg-error "structured bindings in conditions only available with" "" { target c++23_down } }
    {							// { dg-error "switch quantity not an integer" "" { target *-*-* } .-1 }
    case 2:
      break;
    }
  switch (int d = 5; auto [ a, b ] = B ())		// { dg-error "structured bindings in conditions only available with" "" { target c++23_down } }
    {							// { dg-error "switch quantity not an integer" "" { target *-*-* } .-1 }
    case 2:
      break;
    }
  A e = A ();
  auto && [ c, d ] = e;
  auto [ i, j ] = A (), [ k, l ] = A ();		// { dg-error "expected" }
  auto m = A (), [ n, o ] = A ();			// { dg-error "expected" }
}

template <typename T>
auto [ a, b ] = A ();					// { dg-error "expected" }

struct C
{
  auto [ e, f ] = A ();					// { dg-error "expected" }
  mutable auto [ g, h ] = A ();				// { dg-error "expected" }
  virtual auto [ i, j ] = A ();				// { dg-error "expected" }
  explicit auto [ k, l ] = A ();			// { dg-error "expected" }
  friend auto [ m, n ] = A ();				// { dg-error "expected" }
};
