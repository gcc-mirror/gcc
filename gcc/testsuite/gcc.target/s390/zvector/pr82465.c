/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-march=z13 -mzarch -mzvector" } */

/* The vector double variant is available with z13.  A wrong flag in
   the s390-builtins.def file triggered an error when compiling for
   z13.  */

typedef __vector double v2df;

#include <vecintrin.h>

v2df
foo (v2df a)
{
  return vec_sqrt(a);
}
