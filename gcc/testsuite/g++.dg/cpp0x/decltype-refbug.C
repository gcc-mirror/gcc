// { dg-options "-std=c++0x" }
// PR c++/33045
int && f ();

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

static_assert(is_same<decltype(f()), int&&>::value, "decltype of rvalue reference");
