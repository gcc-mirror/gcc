// PR c++/58609
// { dg-do compile { target c++11 } }

struct A
{
  static constexpr int&& i = 0;  // { dg-error "initialization" }
};

int j = A::i;
