// { dg-options "-std=gnu++0x" }
template<typename... Elements> struct tuple {};

template<typename... Args>
struct nested
{
  typedef tuple<tuple<Args, Args...>...> type;
};

template<typename T, typename U>
struct is_same
{
  static const bool value = false;
};

template<typename T>
struct is_same<T, T>
{
  static const bool value = true;
};

int a0[is_same<nested<int, float>::type, 
                      tuple<tuple<int, int, float>, 
                            tuple<float, int, float> > >::value? 1 : -1];
