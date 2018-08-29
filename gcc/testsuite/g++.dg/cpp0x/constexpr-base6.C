// CWG issue 2310
// { dg-do compile { target c++11 } }
// { dg-options "" }

template<typename A, typename B> struct check_derived_from { 
  static A a; 
  static constexpr B *p = &a;	// { dg-error "" }
  int ar[p-p+1];
}; 
struct W { int i; }; 
struct Z : W
{
  check_derived_from<Z, W> cdf;
};
