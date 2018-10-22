// { dg-do compile { target c++17 } }

struct A { bool a, b; };
struct B { int a, b; };

void
foo ()
{
  auto [ a, b ] = A ();
  for (; auto [ a, b ] = A (); )			// { dg-error "expected" }
    ;
  for (; false; auto [ a, b ] = A ())			// { dg-error "expected" }
    ;
  if (auto [ a, b ] = A ())				// { dg-error "expected" }
    ;
  if (auto [ a, b ] = A (); auto [ c, d ] = A ())	// { dg-error "expected" }
    ;
  if (int d = 5; auto [ a, b ] = A ())			// { dg-error "expected" }
    ;
  switch (auto [ a, b ] = B ())				// { dg-error "expected" }
    {
    case 2:
      break;
    }
  switch (int d = 5; auto [ a, b ] = B ())		// { dg-error "expected" }
    {
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
