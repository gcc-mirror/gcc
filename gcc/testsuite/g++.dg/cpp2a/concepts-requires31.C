// PR c++/107179
// { dg-do compile { target c++20 } }

template<bool B> struct bool_constant { static constexpr bool value = B; };

template<typename T>
  struct is_implicitly_default_constructible
  : bool_constant<requires { T(); }>
  { };

struct X { private: X(); };
struct Y { };

static_assert( !is_implicitly_default_constructible<X>::value );
static_assert(  is_implicitly_default_constructible<Y>::value );
