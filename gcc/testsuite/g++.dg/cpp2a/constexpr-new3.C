// P0784R7
// { dg-do compile { target c++2a } }

constexpr int *
f1 ()
{
  return new int (2);		// { dg-error "is not a constant expression because it refers to a result of" }
}

constexpr auto v1 = f1 ();

constexpr bool
f2 ()
{
  int *p = new int (3);		// { dg-error "is not a constant expression because allocated storage has not been deallocated" }
  return false;
}

constexpr auto v2 = f2 ();

constexpr bool
f3 ()
{
  int *p = new int (3);
  int *q = p;
  delete p;
  delete q;			// { dg-error "deallocation of already deallocated storage" }
  return false;
}

constexpr auto v3 = f3 ();	// { dg-message "in 'constexpr' expansion of" }

constexpr bool
f4 (int *p)
{
  delete p;			// { dg-error "deallocation of storage that was not previously allocated" }
  return false;
}

int q;
constexpr auto v4 = f4 (&q);	// { dg-message "in 'constexpr' expansion of" }

constexpr bool
f5 ()
{
  int *p = new int;		// { dg-message "allocated here" }
  return *p == 1;
}

constexpr auto v5 = f5 ();	// { dg-error "the content of uninitialized storage is not usable in a constant expression" }
				// { dg-message "in 'constexpr' expansion of" "" { target *-*-* } .-1 }

constexpr bool
f6 ()
{
  int *p = new int (2);		// { dg-message "allocated here" }
  int *q = p;
  delete p;
  return *q == 2;
}

constexpr auto v6 = f6 ();	// { dg-error "use of allocated storage after deallocation in a constant expression" }
				// { dg-message "in 'constexpr' expansion of" "" { target *-*-* } .-1  }

constexpr int *
f7 ()
{
  int *p = new int (2);		// { dg-error "is not a constant expression because it refers to a result of" }
  delete p;
  return p;
}

constexpr auto v7 = f7 ();
