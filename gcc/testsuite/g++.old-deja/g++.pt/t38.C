// Build don't link: 
// Special g++ Options:
extern "C" int printf (const char *, ...);

template<class X> struct A {
  A (int, char);
  ~A ();
  A (X *, float);
};

template<class Y> inline A<Y>::A (int i, char c) {
  printf ("%d, %d\n", i, c);
}
template<class Z> A<Z>::~A() {}
template<class W> A<W>::A (W * d, float f) {
  printf ("%x, %e\n", d, f);
}

A<void> avoid (9, 0);
