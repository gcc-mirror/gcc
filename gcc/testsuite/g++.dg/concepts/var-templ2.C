// PR c++/67139
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T>
constexpr typename T::type::value_type _v = T::type::value;

template <class T> concept IsTrue_ = _v<T>;

template <class T> concept Unpossible =
  IsTrue_<T &&>;

template <class> constexpr bool unpossible() { return false; }
template<Unpossible T>
constexpr bool unpossible() { return true; }

static_assert((!unpossible<void>()), "");
