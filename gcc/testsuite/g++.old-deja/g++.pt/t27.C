// Build don't link: 

template <class X> int f (X x, int * j) { return 29; }
template <class X> int f (X x, ...) { return 23; }

int foo () {
  return f (7.0, 9.0);
}
