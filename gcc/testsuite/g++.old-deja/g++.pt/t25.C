// { dg-do assemble  }

template <class X> int f (X x, X y) { return 23; }
template <class X> int f (X x, int j = 3) { return 29; }

int foo () {
  return f (7);		// { dg-bogus "" } 
}
