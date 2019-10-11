// { dg-do compile { target concepts } }

template <class T> struct A
{
  static const int x = 42;
};

template <class T> concept R42 = A<T&>::x == 42;

static_assert (!R42<void>);
