// { dg-do compile }
//
// PR 3761

struct A {};

struct B {};

template <class T>
struct Foo : A, B
{
  void func(void);

  struct Nested
  {
    friend void Foo::func(void);
  };
};
