// Testcase for 'this' mangling
// { dg-options -std=c++0x }

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
};

int main()
{
  A a;
  a.f<int>();
  a.g<int>();
}
