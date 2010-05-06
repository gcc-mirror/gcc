// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test template deduction

template <typename T, typename U> struct tType_equal;
template <typename T> struct tType_equal<T, T> { typedef void type; };

template <typename T, typename U>
inline typename tType_equal<T, U>::type
type_equal(U) { }

template<typename T> T* g( T* t );

void test_g()
{
  // Deduction to nullptr_t, no deduction to pointer type
  //
  g(nullptr);               // { dg-error "no matching function for call to " }
  type_equal<float*>(g((float*)nullptr));
}
