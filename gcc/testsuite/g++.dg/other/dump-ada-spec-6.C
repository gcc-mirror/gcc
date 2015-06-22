/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

template<typename T, bool b> class Foo;

template<typename T>
class Foo<T, false>
{
public:
  // This checks that we do not crash on static members from partially
  // specialized class templates.
  static int bar;

  int f();
};

int func()
{
  Foo<int, false> f;
  return f.f();
}

/* { dg-final { cleanup-ada-spec } } */
