/* { dg-do run } */
/* { dg-skip-if "" { powerpc*-*-darwin* powerpc-ibm-aix* } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-O2 -mvsx -mfloat128" } */

/*
 * PR target/99708
 *
 * Verify that __SIZEOF_FLOAT128__ and __SIZEOF_IBM128__ are properly defined.
 */

#include <stdlib.h>

int main (void)
{
  if (__SIZEOF_FLOAT128__ != sizeof (__float128)
  /* FIXME: Once type __ibm128 gets supported with long-double-64,
     we shouldn't need this conditional #ifdef and xfail.  */
#ifdef __SIZEOF_IBM128__
      || __SIZEOF_IBM128__ != sizeof (__ibm128)
#else
      || 1
#endif
     )
    abort ();

  return 0;
}

/* { dg-xfail-run-if "unsupported type __ibm128 with long-double-64" { longdouble64 } } */
