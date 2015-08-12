/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector" } */

/* { dg-final { scan-assembler "nilf\t%r2,15" } } */
/* { dg-final { scan-assembler "vlgvb" } } */

signed char
foo(unsigned char uc)
{
  return __builtin_s390_vec_extract((__vector signed char){ 0 }, uc);
}
