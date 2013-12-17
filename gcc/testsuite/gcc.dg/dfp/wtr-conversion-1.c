/* Test for -Wtraditional warnings on conversions by prototypes.
   Note, gcc should omit these warnings in system header files.
   Based on gcc.dg/wtr-conversion-1.c  */

/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

extern void foo_i (int);
extern void foo_f (float);
extern void foo_ld (long double);
extern void foo_d32 (_Decimal32);
extern void foo_d64 (_Decimal64);
extern void foo_d128 (_Decimal128);

extern int i;
extern float f;
extern long double ld;
extern _Decimal32 d32;
extern _Decimal64 d64;
extern _Decimal128 d128;

void
testfunc1 ()
{
  foo_i (i);
  foo_i (d32); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_i (d64); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_i (d128); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_d32 (i); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_d32 (f); /* { dg-warning "as '_Decimal32' rather than 'float'" "prototype conversion warning" } */
  foo_d32 (ld); /* { dg-warning "as '_Decimal32' rather than 'long double'" "prototype conversion warning" } */
  foo_d32 (d64); /* { dg-warning "as '_Decimal32' rather than '_Decimal64'" "prototype conversion warning" } */
  foo_d32 (d128); /* { dg-warning "as '_Decimal32' rather than '_Decimal128'" "prototype conversion warning" } */
  foo_d64 (i); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_d64 (f); /* { dg-warning "as '_Decimal64' rather than 'float'" "prototype conversion warning" } */
  foo_d64 (ld); /* { dg-warning "as '_Decimal64' rather than 'long double'" "prototype conversion warning" } */
  foo_d64 (d32); /* { dg-bogus "as '_Decimal64' rather than '_Decimal32'" "prototype conversion warning" } */
  foo_d64 (d128); /* { dg-warning "as '_Decimal64' rather than '_Decimal128'" "prototype conversion warning" } */
  foo_d128 (i); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_d128 (f); /* { dg-warning "as '_Decimal128' rather than 'float'" "prototype conversion warning" } */
  foo_d128 (ld); /* { dg-warning "as '_Decimal128' rather than 'long double'" "prototype conversion warning" } */
  foo_d128 (d32); /* { dg-bogus "as '_Decimal128' rather than '_Decimal32'" "prototype conversion warning" } */
  foo_d128 (d64); /* { dg-bogus "as '_Decimal128' rather than '_Decimal64'" "prototype conversion warning" } */
  foo_d128 (d128); /* { dg-bogus "as '_Decimal128' rather than '_Decimal'" "prototype conversion warning" } */
}
  
# 54 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

void
testfunc2 ()
{
  foo_i (i);
  foo_i (d32);
  foo_i (d64);
  foo_i (d128);
  foo_d32 (i);
  foo_d32 (f);
  foo_d32 (ld);
  foo_d32 (d32);
  foo_d32 (d64);
  foo_d32 (d128);
  foo_d64 (i);
  foo_d64 (f);
  foo_d64 (ld);
  foo_d64 (d32);
  foo_d64 (d64);
  foo_d64 (d128);
  foo_d128 (i);
  foo_d128 (f);
  foo_d128 (ld);
  foo_d128 (d32);
  foo_d128 (d64);
  foo_d128 (d128);
}
