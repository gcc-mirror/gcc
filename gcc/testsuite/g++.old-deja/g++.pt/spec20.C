// Build don't link:

// According to [temp.class.spec.mfunc]/2, these are valid

template <class T> 
struct S {
  template <class U> void f(U);
  template <> void f<int>(int); // gets bogus error - XFAIL *-*-*

  template <class V> struct I {};
  template <class V> struct I<V*> {};
  template <> struct I<int>; // gets bogus error - XFAIL *-*-*
};
