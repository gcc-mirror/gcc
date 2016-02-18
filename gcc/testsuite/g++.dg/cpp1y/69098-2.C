// PR c++/69098
// { dg-do compile { target c++14 } }

struct A
{
  template <int>
  static void *pf;
};

template <typename B>
bool foo1 () {
  return A::pf<false>;
}

template <typename B>
bool foo2 () {
  return B::template pf<false>;
}

template <typename B>
bool foo3 () {
  return &A::pf<false>;
}

template <typename B>
bool foo4 () {
  return &B::template pf<false>;
}


void bar () {
  foo1<A>();
  foo2<A>();
  foo3<A>();
  foo4<A>();
}

