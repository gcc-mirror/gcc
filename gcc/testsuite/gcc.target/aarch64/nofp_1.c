/* { dg-skip-if "conflicting -march" { *-*-* } { "-march=*" } { "-march=*+nofp" } } */
/* If there are multiple -march's, the latest wins; skip the test either way.
   -march overrides -mcpu, so there is no possibility of conflict.  */

/* { dg-options "-march=armv8-a+nofp" } */

#include <stdarg.h>

typedef int int32x2_t __attribute__ ((__vector_size__ ((8))));

int test (int i, ...);

int
main (int argc, char **argv)
{
  int32x2_t a = (int32x2_t) {0, 1};
  int32x2_t b = (int32x2_t) {2, 3};
  return test (2, a, b); /* { dg-error "'\\+nofp' feature modifier is incompatible with the use of vector types" } */
}
