// { dg-do compile { target c++11 } }

namespace C
{
  void f();
}

namespace B
{
  using namespace C;

  inline namespace B1
  {
    void f();
  }
}

namespace A
{
  using namespace B;
}

int main()
{
  A::f();
}
