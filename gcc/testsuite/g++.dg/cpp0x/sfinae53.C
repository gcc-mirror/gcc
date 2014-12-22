// PR c++/59204
// { dg-do compile { target c++11 } }

template< class T >
  using void_t = void;

template< class T, class = void >
  struct has_type
{ constexpr static bool value = false; };

template< class T >
  struct has_type<T, void_t<typename T::type>>
{ constexpr static bool value = true; };

struct yes  { using type = int; };
struct no   { };

int
main( )
{
  static_assert(     has_type<yes>::value, "false negative!" );
  static_assert( not has_type<no >::value, "false positive!" );
}
