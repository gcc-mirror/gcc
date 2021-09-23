/* Tests for CTF float base types.
   - Verify that there is a single record for the base types.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler-times "ascii \"float.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"double.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"long double.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

float a;
float b = 33;

double c = 44;
double d = 45;

long double e;
