// Build don't link:

// According to the non-normative example in
// [temp.class.spec.mfunc]/2, these should be valid, but the grammar
// in the Standard does not allow partial nor full specializations as
// member-declarations, so we'd better not support them.

template <class T> 
struct S {
  template <class U> void f(U);
  template <> void f<int>(int); // ERROR - invalid specialization

  template <class V> struct I {};
  template <class V> struct I<V*> {};
  template <> struct I<int>; // ERROR - invalid specialization
};
