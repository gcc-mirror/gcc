// { dg-do compile { target { powerpc*-*-linux* } } }
// { dg-require-effective-target ppc_float128_sw }
// { dg-options "-mvsx -mfloat128 -O2 -mabi=ibmlongdouble -Wno-psabi" }

// PR 85657
// Check that __ibm128 and long double can be used in the same template,
// even if long double uses the IBM extended double representation.

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

  __asm__ (" # %x0, %x1" : "+d" (ld), "+d" (ibm));

  return iszero (ld) + iszero (ibm);
}
