// PR c++/67139
// { dg-options "-std=c++1z -fconcepts" }

template <class T>
constexpr typename T::type::value_type _v = T::type::value;

template <class T> concept bool IsTrue_() { return _v<T>; }

template <class T> concept bool Unpossible() {
  return IsTrue_<T &&>();
}

template <class> constexpr bool unpossible() { return false; }
Unpossible{ T }
constexpr bool unpossible() { return true; }

static_assert((!unpossible<void>()), "");
