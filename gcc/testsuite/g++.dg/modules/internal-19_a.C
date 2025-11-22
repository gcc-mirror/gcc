// PR c++/122636
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;
export template <typename>
struct Foo {
  constexpr static inline auto lambda = []{};
  template <typename T = decltype(lambda)>
  static void foo(T = lambda) {}
};

export template <typename... Types>
struct Type
{
  template <typename T>
  auto test(T to)
  {
    return [to](auto && ...){ return to; }();
  }
};
