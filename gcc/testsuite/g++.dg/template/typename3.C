// { dg-do compile }
// crash test - PR 7266

template <class A>
struct B {
 typedef A::C::D E;  // { dg-error "no type|(parse|syntax) error" }
};
