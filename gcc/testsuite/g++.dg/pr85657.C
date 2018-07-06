// { dg-do compile { target { powerpc*-*-linux* } } }
// { dg-require-effective-target ppc_float128_sw }
// { dg-options "-mvsx -mfloat128 -O2 -mabi=ibmlongdouble -Wno-psabi" }

// PR 85657
// Check that __ibm128 and long double are represented as different types, even
// if long double is currently using the same representation as __ibm128.

template <class __T> inline bool
iszero (__T __val)
{
  return __val == 0;
}

int
use_template (void)
{
  long double ld = 0.0;
  __ibm128 ibm = 0.0;

#ifdef _ARCH_PWR7
  __asm__ (" # %x0, %x1" : "+d" (ld), "+d" (ibm));
#endif

  return iszero (ld) + iszero (ibm);
}

class foo {
public:
  foo () {}
  ~foo () {}
  inline bool iszero (long double ld) { return ld == 0.0; }
  inline bool iszero (__ibm128 i128) { return i128 == 0.0; }
} st;

int
use_class (void)
{
  long double ld = 0.0;
  __ibm128 ibm = 0.0;

#ifdef _ARCH_PWR7
  __asm__ (" # %x0, %x1" : "+d" (ld), "+d" (ibm));
#endif

  return st.iszero (ld) + st.iszero (ibm);
}
