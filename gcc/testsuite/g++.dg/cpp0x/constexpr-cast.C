// Test to verify that evaluating reinterpret_cast is diagnosed in
// constant expressions.
// { dg-do compile { target c++11 } }

int i;

// The following is accepted due to bug 49171.
constexpr void *q = reinterpret_cast<void*>(&i);    // { dg-error "" "bug c++/49171" { xfail *-*-*-* } }

constexpr void *r0 = reinterpret_cast<void*>(1);    // { dg-error "not a constant expression" }
constexpr void *r1 = reinterpret_cast<void*>(sizeof 'x');  // { dg-error ".reinterpret_cast<void\\*>\\(1ul\\). is not a constant-expression" }

template <class T>
constexpr bool f ()
{
#if __cplusplus > 201103L
  T *p = reinterpret_cast<T*>(sizeof (T));
  return p;
#else
  return *reinterpret_cast<T*>(sizeof (T));
#endif
}

constexpr bool b = f<int>();   // { dg-error "not a constant expression" }
