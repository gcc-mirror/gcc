// Build don't link:

template <class T> 
struct S {
  template <class U> void f(U);
  template <> void f<int>(int); // ERROR - specialization

  template <class V> struct I {};
  template <class V> struct I<V*> {};
  template <> struct I<int>; // ERROR - specialization
};
