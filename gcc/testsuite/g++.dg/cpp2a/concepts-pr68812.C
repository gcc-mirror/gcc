// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

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
  template<typename X, typename Y> concept Foo = true;

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

