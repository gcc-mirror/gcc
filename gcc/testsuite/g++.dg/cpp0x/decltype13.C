// PR c++/34269
// { dg-do compile }

void
f1 ()
{
  __decltype;		// { dg-error "expected" }
}

void
f2 ()
{
  __decltype (;		// { dg-error "expected" }
}

void
f3 ()
{
  __decltype ();	// { dg-error "expected" }
}

void
f4 ()
{
  __typeof__;		// { dg-error "expected" }
}

void
f5 ()
{
  __typeof__ (;		// { dg-error "expected" }
}

void
f6 ()
{
  __typeof__ ();	// { dg-error "expected" }
}
