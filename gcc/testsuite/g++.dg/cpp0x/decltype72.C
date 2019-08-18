// PR c++/67853
// { dg-do compile { target c++11 } }

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

struct Member {};
struct A { Member x; };
A MakeA();
static_assert(is_same<decltype((MakeA().x)), Member&&>::value, "");
