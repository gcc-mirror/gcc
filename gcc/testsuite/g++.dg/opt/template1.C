// { dg-options "-O2" }
// { dg-final { scan-assembler-not "\n_?_ZN1AILi0EE4foo1Ev\[: \t\n\]" } }

template <int>
struct A {
    void foo1 () throw ();
    void foo2 ();

    void UNRELATED ();
};

template <> void A<0>::UNRELATED ();

template <int dim> inline void A<dim>::foo1 () throw () {}
template <int dim> inline void A<dim>::foo2 ()          {}

void bar (A<0> &a) {
  a.foo1 ();
}
