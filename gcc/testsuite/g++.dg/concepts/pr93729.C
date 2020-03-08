// { dg-do compile { target c++2a } }

// PR c++/93729

struct B
{
  int a:4;
  int b:4;
};

template<typename T>
concept c1
  = requires(T x, void(f)(int &)) { f(x.a); }; // { dg-bogus "cannot bind" }

static_assert(!c1<B>);
