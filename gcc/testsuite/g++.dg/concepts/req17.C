// PR c++/67018
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <typename T>
constexpr bool Val = true;

template <class I>
concept bool InputIterator = requires (I i) {
  requires Val <decltype(i++)>;
};

template <class I>
concept bool ForwardIterator = InputIterator<I> && true;

template<InputIterator>
constexpr bool f() { return false; }
template<ForwardIterator>
constexpr bool f() { return true; }

static_assert(f<int*>());
