/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -mx87regparm" } */
/* { dg-require-effective-target ilp32 } */

float efoo_f(float);
double efoo_d(double);
long double efoo_ld(long double);

volatile float f;
volatile double d;
volatile long double ld;

void test() 
{
  f = efoo_f(f);
  d = efoo_d(d);
  ld = efoo_ld(ld);
}

/* Check that no memory is used to pass arguments.  */

/* { dg-final { scan-assembler-not "\\(%esp\\)" { xfail *-*-* } } } */
