// Testcase for 'this' mangling
// { dg-do compile { target c++11 } }

struct B
{
  template <class U> U f();
};

struct A
{
  B b;
  // { dg-final { scan-assembler "_ZN1A1fIiEEDTcldtdtdefpT1b1fIT_EEEv" } }
  template <class U> auto f() -> decltype (b.f<U>());
  // { dg-final { scan-assembler "_ZN1A1gIiEEDTcldtptfpT1b1fIT_EEEv" } }
  template <class U> auto g() -> decltype (this->b.f<U>());
  // { dg-final { scan-assembler "_ZN1A1hIiEEDTcldtdtdefpT1bsr1B1fIT_EEEv" } }
  template <class U> auto h() -> decltype (b.B::f<U>());
  // { dg-final { scan-assembler "_ZN1A1iIiEEDTcldtptfpT1bsr1B1fIT_EEEv" } }
  template <class U> auto i() -> decltype (this->b.B::f<U>());
};

int main()
{
  A a;
  a.f<int>();
  a.g<int>();
  a.h<int>();
  a.i<int>();
}
