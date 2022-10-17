// { dg-do run { target c++11 } }

struct B { B() {} ~B() noexcept(false) { throw 42; } };
int a;
struct A { A() { ++a; }; A(B) { ++a; } ~A() { --a; } };

using Arr = A[3];

int main()
{
  try {
    auto&& ref = Arr{B()};
  } catch (...) { }
  return a;
}
