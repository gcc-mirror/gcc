// Test that -Wparentheses does not give bogus warnings in the
// presence of templates for non-plain assignment.  Bug 17120.

// { dg-do compile }
// { dg-options "-Wparentheses" }

template<typename _Tp>
  inline _Tp
  cmath_power(_Tp __x, unsigned int __n)
  {
    while (__n >>= 1)
      ;
    return __x;
  }

int main()
{
  cmath_power(1.0, 3);
}
