// { dg-do compile { target c++11 } }
template<typename T, T a, T... Params>
struct max
{
  static const T value = a > max<T, Params>::value ? a : max<T, Params>::value; // { dg-error "not expanded|Params" }
};

template<typename T, T a, T b>
struct max<T, a, b>
{
        static const T value = a > b ? a : b;
};

static const int value1 = max< int, 1, 2>::value;
static const int value2 = max< int, 1, 3, 5>::value;
