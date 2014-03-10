// { dg-do compile { target c++1y } }
// { dg-final { scan-assembler "_ZN1AIiEcvDaEv" } }

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
