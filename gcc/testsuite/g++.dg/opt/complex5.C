// PR 24365
// { dg-do compile }
// { dg-options "-O2" }

typedef __complex__ double cdouble;
inline cdouble to_complex(double r) {
  cdouble z;
  __real__ z = r;
  return z;
}
cdouble elt_zero() {
  cdouble a = to_complex(0.0);
  a+=1.0;
  return a;
}

