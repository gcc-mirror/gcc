// CWG issue 2310
// { dg-do compile { target c++11 } }

template<typename A, typename B> struct check_derived_from { 
  static A a; 
  static constexpr B *p = &a;	// { dg-error "cannot convert" }
  int ar[p-p+1];  // { dg-error "13:size of array is not an integral constant-expression" }
}; 
struct W { int i; }; 
struct Z : W
{
  check_derived_from<Z, W> cdf;
};
