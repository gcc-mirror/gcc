// PR c++/88631
// { dg-do compile { target c++17 } }

template<class T = void>
class A { };

int main()
{
  auto x = A();
  auto x2 = A{};
  A y;
}
