// PR c++/95808
// { dg-do compile { target c++20 } }

constexpr
bool foo ()
{
  int *p = new int;	// { dg-message "allocation performed here" }
  delete[] p;		// { dg-error "array deallocation of object allocated with non-array allocation" }
  return false;
}

constexpr
bool bar ()
{
  int *p = new int[1];	// { dg-message "allocation performed here" }
  delete p;		// { dg-error "non-array deallocation of object allocated with array allocation" }
  return false;
}

constexpr auto x = foo ();
constexpr auto y = bar ();
