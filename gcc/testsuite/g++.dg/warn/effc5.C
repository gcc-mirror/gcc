// PR c++/98841
// { dg-do compile }
// { dg-options "-Weffc++" }

struct S {
  template <typename T>
  S& operator=(const T&) { return *this; }	// { dg-bogus "should return a reference to" }
  S& operator=(const S&) { return *this; }
};

void
foo ()
{
  S s, t;
  s = 1;
  s = t;
}
