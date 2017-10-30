// PR c++/78283
// { dg-do compile }
// { dg-options "-Wall" }

void foo () throw () {}		// { dg-bogus "mangled name" }

template <class T>
T bar (T x) { return x; }

void baz () {			// { dg-bogus "mangled name" }
  return (bar (foo)) ();
}

void decl () {}			// { dg-bogus "mangled name" }
