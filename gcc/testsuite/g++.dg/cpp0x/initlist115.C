// PR c++/89852
// { dg-do compile { target c++11 } }

struct A {
  int b;
};

struct B {
  A g;
};

const auto j = A{};

template <typename>
void k()
{
  B{j};
}
