// Build don't link: 

template <class X> int f (X x, unsigned int j = 3) { return 29; }
template <class X> int f (X x, X y) { return 23; }

int foo () {
  return f (7.0, 9.0);
}
