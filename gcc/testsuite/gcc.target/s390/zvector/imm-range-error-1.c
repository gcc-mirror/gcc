/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector" } */

#include <vecintrin.h>

__vector unsigned char q;
__vector unsigned short int h;
__vector unsigned int s;
__vector unsigned long long d;

int
main ()
{
  vec_msum_u128 (d, d, q, 5); /* { dg-error "constant argument 4 for builtin '__builtin_s390_vec_msum_u128' is invalid \\(0, 4, 8, 12\\)" } */

  /* Using the resolved low-level builtins here makes the errors to be
     triggered from s390_expand_builtin.  Otherwise they would come
     from the parser already preventing other errors from showing
     up.  */
  __builtin_s390_vrepb (q, 17); /* { dg-error "constant argument 2 for builtin '__builtin_s390_vrepb' is out of range \\(0..15\\)" } */
  __builtin_s390_vreph (h,  8); /* { dg-error "constant argument 2 for builtin '__builtin_s390_vreph' is out of range \\(0..7\\)" } */
  __builtin_s390_vrepf (s,  4); /* { dg-error "constant argument 2 for builtin '__builtin_s390_vrepf' is out of range \\(0..3\\)" } */
  __builtin_s390_vrepg (d,  2); /* { dg-error "constant argument 2 for builtin '__builtin_s390_vrepg' is out of range \\(0..1\\)" } */

  __builtin_s390_vpdi (d, d, 4); /* { dg-error "constant argument 3 for builtin '__builtin_s390_vpdi' is out of range \\(0..3\\)" } */
}
