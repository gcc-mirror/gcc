/* Tests for BTF floating point type kinds. We expect a single record for each
   of the base types: float, double and long double.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x10000000\[\t \]+\[^\n\]*btt_info" 3 } } */

/* { dg-final { scan-assembler-times "ascii \"float.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"double.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"long double.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */

float a;
float b = 1.5f;

double c;
double d = -99.9;

long double e;
long double f = 1000.01;
