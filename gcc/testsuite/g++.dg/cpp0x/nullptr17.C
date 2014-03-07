// { dg-do compile { target c++11 } }

// Test that bool is a better overload match than int

template <typename T, typename U> struct tType_equal;
template <typename T> struct tType_equal<T, T> { typedef void type; };

template <typename T, typename U>
inline typename tType_equal<T, U>::type
type_equal(U) { }

int i( int );
long int i( long int );
bool i( bool );

void test_i()
{
  // Overload to bool, not int
  type_equal<bool>(i(nullptr));
  decltype(nullptr) mynull = 0;
  type_equal<bool>(i(mynull));
}
