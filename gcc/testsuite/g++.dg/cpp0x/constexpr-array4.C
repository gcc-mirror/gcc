// PR c++/49924
// { dg-do compile { target c++11 } }

struct A { constexpr A() { } };

struct B {
  A array[1]; //non-static member array of a literal type w constexpr ctor
  constexpr B() : array{} { } // here is the problem
};

int main()
{
  constexpr B b{};  // won't compile
}
