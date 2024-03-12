// PR debug/109676
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -g -march=alderlake" }

template <class T>
struct A {
  T a;
  char b;
  template <typename U>
  A (U x, int) : a{x} {}
  A (...);
  T foo () { return a; }
};
bool bar ();
struct B { int c, d; unsigned char e[8]; };
bool baz ();
struct C { C () : f () {} B &boo () { return f; } B f; };

A<B>
qux ()
{
  {
    A<B> p;
    bool t = true;
    for (; bar ();)
      if (baz ())
	{
	  t = false;
	  break;
	}
    if (t)
      p.b = false;
    return p;
  }
}

A<C>
garply ()
{
  C g;
  A<B> h = qux ();
  if (!h.b)
    return 0;
  g.boo () = h.foo ();
  return A<C>{g, 0};
}
