// { dg-do compile { target c++11 } }
// { dg-options -Wabi=10 }

struct [[gnu::abi_tag ("foo")]] A
{
  template <class T> static T f();
  template <class T> static A g();
};

template <class T> struct B
{
  static decltype(A::f<T>()) fa(decltype(A::f<T>()));
  static decltype(A::f<T>()) fv(); // { dg-warning "mangled name" }
  static decltype(A::g<T>()) ga(decltype(A::g<T>()));
  static decltype(A::g<T>()) gv();
  template <class U> 
  static decltype(A::f<U>()) hv();
};

int main()
{
  B<int>::fa(0);     // { dg-final { scan-assembler "_ZN1BIiE2faEi" } }
  B<int>::fv();	     // { dg-final { scan-assembler "_ZN1BIiE2fvEv" } }
  B<int>::ga(A());   // { dg-final { scan-assembler "_ZN1BIiE2gaE1AB3foo" } }
  B<int>::gv();	     // { dg-final { scan-assembler "_ZN1BIiE2gvB3fooEv" } }
  B<int>::hv<int>(); // { dg-final { scan-assembler "_ZN1BIiE2hvIiEEDTclsr1AB3foo1fIT_EEEv" } }
}
