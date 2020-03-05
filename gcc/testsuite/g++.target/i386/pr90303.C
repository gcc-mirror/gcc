// PR tree-optimization/90303
// { dg-do compile { target ia32 } }
// { dg-additional-options "-O2" }

struct A { virtual void foo (); };
template <class> class B : A {};
typedef void (__attribute__((fastcall)) F) ();
B<F> e;
