/* Tests for CTF complex base types.

   CTF does not have representation for complex integer types.
   
   This testcase has a mix of C constructs containing COMPLEX_TYPE.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-require-effective-target libc_has_complex_functions } */

/* { dg-final { scan-assembler-times "ascii \"complex double.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"complex long double.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"complex float.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

#include <complex.h>

double complex z1 = I * I;

const long double complex z2 = I * I;

float complex z4 = 1+2.11*I;
