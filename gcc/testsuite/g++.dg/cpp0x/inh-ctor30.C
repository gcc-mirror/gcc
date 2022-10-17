// PR c++/81860
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZN1AIjEC\[12\]Ev" } }
// { dg-additional-options -fno-implicit-constexpr }

template <typename T>
struct A
{
  A() {}
};

struct B
{
  template <typename D>
  B(D, const A<unsigned>& a = A<unsigned>()) : a(a) {}

  A<unsigned> a;
};

struct C : B
{
  using B::B;
};

int main()
{
  C c(0);
}
