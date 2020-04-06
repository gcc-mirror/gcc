// PR c++/90995
// { dg-do compile { target c++11 } }

void
foo ()
{
  enum : int a alignas;		// { dg-error "expected" }
}

void
bar ()
{
  enum : int a;			// { dg-error "expected" }
}

void
baz ()
{
  enum class a : int b alignas;	// { dg-error "expected" }
}

void
qux ()
{
  enum class a : int b;		// { dg-error "expected" }
}
