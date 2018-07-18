/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-march=z13 -mzarch -mzvector" } */

#include <vecintrin.h>

void
foo(signed char *p, int i) {
  vec_load_bndry(p, i); /* { dg-error "constant value required for builtin.*2" } */
}
