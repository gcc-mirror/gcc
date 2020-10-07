// PR c++/88115
// { dg-do compile { target c++11 } }

template<int __v>
struct integral_constant {
  static constexpr int value = __v;
};

template <class T> using StdAlignOf = integral_constant<alignof(T)>;
template <class T> using GCCAlignOf = integral_constant<__alignof__(T)>;

static_assert(StdAlignOf<double>::value == alignof(double), "");
static_assert(GCCAlignOf<double>::value == __alignof__(double), "");
