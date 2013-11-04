// { dg-do compile }
// { dg-options "-std=c++11" }

// Test template deduction

typedef decltype(nullptr) nullptr_t;

template <typename T, typename U> struct tType_equal;
template <typename T> struct tType_equal<T, T> { typedef void type; };

template <typename T, typename U>
inline typename tType_equal<T, U>::type
type_equal(U) { }

template<typename T> T h( T t );

void test_h()
{
  type_equal<int>(h(0));
  type_equal<nullptr_t>(h(nullptr));
  type_equal<float*>(h((float*)nullptr));
  nullptr_t mynull = 0;
  type_equal<nullptr_t>(h(mynull));
  type_equal<float*>(h((float*)mynull));
}
