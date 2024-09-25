// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

namespace X
{
  template<class> constexpr bool x = true;
}

template<int> using helper = void;

template<typename T>
concept C =
  requires
  {
    requires X::x<T>;
    typename helper<T{}>;
  };

static_assert(C<int>);
