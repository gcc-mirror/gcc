// Test unifying SCOPE_REF.
// Origin: Marc Duflot <m.duflot@ulg.ac.be>
// { dg-do compile }

template <int n> class A {};
template <int m> class R {};

template <int n> struct Trait { enum {m = n}; };

template <int n> R<Trait<n>::m> f(A<n>);
template <> R<1> f(A<1>) {return R<1>();}
