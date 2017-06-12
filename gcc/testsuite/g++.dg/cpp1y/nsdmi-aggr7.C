// PR c++/79797
// { dg-do compile { target c++14 } }

struct A
{
  A* x[1]{(A*)this};
};

extern constexpr A a{};

#define SA(X) static_assert ((X), #X)
SA (a.x[0] == &a);
