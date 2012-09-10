// PR c++/50545
// { dg-do compile { target c++11 } }

template< class T >
T&& declval();

// #1
template< class T >
auto f( int )
  -> decltype( int{ declval<T>() } );

// #2
template< class >
void f( ... );


#define STATIC_ASSERT( ... ) static_assert( __VA_ARGS__, #__VA_ARGS__ )

template< class T, class U >
struct is_same {
  static constexpr bool value = false;
};

template< class T >
struct is_same<T, T> {
  static constexpr bool value = true;
};


STATIC_ASSERT( is_same< decltype( f<int>(0) ),  int >::value );  // OK; f<int>(0) calls #1.
STATIC_ASSERT( is_same< decltype( f<int*>(0) ), void >::value ); // static assertion fails; f<int*>(0) should call #2, because int{ (int*)0 } is ill-formed, but calls #1.
