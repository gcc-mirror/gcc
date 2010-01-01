/* Test syntax of hexadecimal floating point constants: at least one
   digit needed before or after point.  PR 41947.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

double d0 = 0x0.0p0;
double d1 = 0x.0p0;
double d2 = 0x0.p0;
double d3 = 0x0.0p+0;
double d4 = 0x.0p+0;
double d5 = 0x0.p+0;
double d6 = 0x0.0p-0;
double d7 = 0x.0p-0;
double d8 = 0x0.p-0;

double e0 = 0x.p0; /* { dg-error "no digits" } */
double e1 = 0x0.; /* { dg-error "require an exponent" } */
double e2 = 0x.0; /* { dg-error "require an exponent" } */
double e3 = 0x0.0; /* { dg-error "require an exponent" } */
double e4 = 0x0.0p; /* { dg-error "exponent has no digits" } */
double e5 = 0x0.0pf; /* { dg-error "exponent has no digits" } */
double e6 = 0x0.0p+; /* { dg-error "exponent has no digits" } */
double e7 = 0x0.0p+f; /* { dg-error "exponent has no digits" } */
double e8 = 0x0.0p-; /* { dg-error "exponent has no digits" } */
double e9 = 0x0.0p-f; /* { dg-error "exponent has no digits" } */
