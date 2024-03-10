// PR c++/90995
// { dg-do compile { target c++11 } }

void
foo ()
{
  enum : int a alignas;		// { dg-error "declaration of enum" }
  // { dg-error {expected '\(' before ';'} "" { target *-*-* } .-1 }
}

void
bar ()
{
  enum : int a;			// { dg-error "declaration of enum" }
}

void
baz ()
{
  enum class a : int b alignas;	// { dg-error "declaration of enum" }
  // { dg-error {expected '\(' before ';'} "" { target *-*-* } .-1 }
}

void
qux ()
{
  enum class a : int b;		// { dg-error "declaration of enum" }
}
