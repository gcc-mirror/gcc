// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-ts" }

namespace X
{
  template<class> constexpr bool x = true;
}

template<int> using helper = void;

template<typename T>
concept bool C =
  requires
  {
    requires X::x<T>;
    typename helper<T{}>;
  };

static_assert(C<int>);
