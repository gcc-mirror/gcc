// PR c++/91911
// { dg-do compile { target c++17 } }

template<class T>
struct span {
  using value_type = T;
  span(T);
};

template<class T>
using SpanType = decltype(span{T()});

template<class T>
using ConstSpanType = span<const typename SpanType<T>::value_type>;

using type = ConstSpanType<int>;
using type = span<const int>;
