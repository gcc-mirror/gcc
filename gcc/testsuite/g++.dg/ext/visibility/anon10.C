// http://bugzilla.redhat.com/411871
// { dg-do compile }

extern "C" int printf (const char *, ...);

struct E
{
  template <typename T> E (const volatile T&);
  template <typename T> E (T&);
  char x[64];
};

template<typename T> struct D
{
  static E foo (E, ...);
  static int foo (T, int);
};

template<typename T, typename U> struct C
{
  static T ca;
  static const int value = sizeof (D<U>::foo (ca, 0)) == sizeof (int);
};

struct A
{
  int a;
};

namespace
{
  struct B
  {
    int a;
  };
}

int bar (void)
{
  C<A, int> a;
  C<B, int> b;

  return a.value + b.value;
}
