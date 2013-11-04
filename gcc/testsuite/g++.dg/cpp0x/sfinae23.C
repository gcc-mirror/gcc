// PR c++/48647
// { dg-options -std=c++11 }

template< class T >
T&& declval();

template< class T, class U >
decltype( true ? declval<T>() : declval<U>() ) test( int );

template< class T, class U >
void test( ... );


template< class T, class U >
struct is_same {
  static const bool value = false;
};

template< class T >
struct is_same<T, T> {
  static const bool value = true;
};

#define SA(X) static_assert ((X),#X)

typedef decltype( test<int*, double*>(0) ) void_expected;
SA ((is_same<void_expected, void>::value));
SA ((!is_same<void_expected, void*>::value));
