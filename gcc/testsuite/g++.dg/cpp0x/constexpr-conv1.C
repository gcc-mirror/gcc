// PR c++/56041
// { dg-do compile { target c++11 } }

template< class T, T v >
struct integral_constant
{
  using type       = integral_constant<T,v>;
  using value_type = T;
  static constexpr T value  = v;
  constexpr operator T  ( )  noexcept { return value; }
};

using true_type  = integral_constant<bool, true>;
using false_type = integral_constant<bool, false>;

template< bool b, class T = void >  struct enable_if  { using type = T; };
template< class T >                 struct enable_if<false, T>  { };


template< class T,
	  class = typename enable_if< true_type{}       // should compile; doesn't
				      , T>::type
	  >
T try_it( )  { return T{}; }

int main( )
{
  static_assert( true_type{}     , "failed test 1!" );
  static_assert( true_type{}     , "failed test 2!" );
  static_assert( ! false_type{}  , "failed test 3!" );
  static_assert( !! true_type{}  , "failed test 4!" );

  return try_it<int>();
}
