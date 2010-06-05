// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test overload of pointer versus bool when applied on a nullptr_t

template <typename T, typename U> struct tType_equal;
template <typename T> struct tType_equal<T, T> { typedef void type; };

template <typename T, typename U>
inline typename tType_equal<T, U>::type
type_equal(U) { }

char* j( char* );
bool j(  bool );

void test_j()
{
  type_equal<char*>(j(nullptr));
  decltype(nullptr) mynull = 0;
  type_equal<char*>(j(mynull));
}
