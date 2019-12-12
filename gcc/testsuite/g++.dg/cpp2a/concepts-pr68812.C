// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

namespace zero
{
  template<int... s>
  struct S
  {
    template<int... f>
      requires(... and (s == f))
    static void F()
    {
    }
  };

  void foo(S<>) {}
}

namespace one
{
  template<typename X, typename Y> concept bool Foo = true;

  template<typename... T>
  struct foo
  {
    template<typename... U>
    foo(U...)
      requires (Foo<T, U> && ...)
    {}
  };

  void bar(foo<int, long, double>) {}
}

