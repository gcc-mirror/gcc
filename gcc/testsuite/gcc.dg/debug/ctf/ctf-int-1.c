/* Tests for CTF integer base types.
   - Verify that there is a single record for the base types.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler-times "ascii \"int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"short int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"long int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

int a;
int b = 33;

short int c = 44;
short int d = 45;

long int e = 90;
long int f;
