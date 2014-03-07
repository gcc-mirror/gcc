// { dg-do compile { target c++11 } }

// Test overload preference char*/int

template <typename T, typename U> struct tType_equal;
template <typename T> struct tType_equal<T, T> { typedef void type; };

template <typename T, typename U>
inline typename tType_equal<T, U>::type
type_equal(U) { }

char* f( char* );
int f( int );
long int f( long int );

void test_f()
{
  // Overloading cases
  //
  type_equal<char*>(f(nullptr));
  type_equal<int>(f(0));
  decltype(nullptr) mynull = 0;
  type_equal<char*>(f(mynull));
}
