// PR c++/67018
// { dg-do compile { target c++20 } }

template <typename T>
constexpr bool Val = true;

template <class I>
concept InputIterator = requires (I i) {
  requires Val<decltype(i++)>;
};

template <class I>
concept ForwardIterator = InputIterator<I> && true;

template<InputIterator>
constexpr bool f() { return false; }

template<ForwardIterator>
constexpr bool f() { return true; }

static_assert(f<int*>());
