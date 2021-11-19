// { dg-do compile { target c++14 } }
// { dg-final { scan-assembler "_ZN1AIiEcvDaEv" } }
// { dg-additional-options -fno-implicit-constexpr }

template <class T>
struct A {
  T t;
  operator auto() { return t+1; }
};

int main()
{
  int i = A<int>{42};
  return (i != 43);
}
