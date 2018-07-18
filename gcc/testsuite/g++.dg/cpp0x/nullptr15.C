// { dg-do compile { target c++11 } }


// Test template deduction

template <typename T, typename U> struct tType_equal;
template <typename T> struct tType_equal<T, T> { typedef void type; };

template <typename T, typename U>
inline typename tType_equal<T, U>::type
type_equal(U) { }

template<typename T> T* g( T* t ); // { dg-message "note" }

void test_g()
{
  // Deduction to nullptr_t, no deduction to pointer type
  //
  g(nullptr);               // { dg-error "no matching function for call to " }
  // { dg-message "(candidate|mismatched types)" "candidate note" { target *-*-* } .-1 }
  type_equal<float*>(g((float*)nullptr));
  decltype(nullptr) mynull = 0;
  g(mynull);                // { dg-error "no matching function for call to " }
  // { dg-message "(candidate|mismatched types)" "candidate note" { target *-*-* } .-1 }
  type_equal<float*>(g((float*)mynull));
}
