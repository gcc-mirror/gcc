// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-g -fdebug-types-section" }
class A {
public:
  A();
  template <typename U> A(U);
};
template <class> struct B { typedef A type; };
template <class R, typename... Args>
int Bind(R(Args...), typename B<Args>::type...) { return 0; }
void KeepBufferRefs(A, A) { A a, b(Bind(KeepBufferRefs, a, b)); }
