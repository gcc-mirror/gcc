/* Test for -Wtraditional warnings on conversions by prototypes.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 4/09/2001.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

extern void foo_c (char);
extern void foo_ll (long long);
extern void foo_f (float);
extern void foo_ld (long double);
extern void foo_cd (__complex__ double);

extern char c;
extern long long ll;
extern float f;
extern long double ld;
extern __complex__ double cd;

void
testfunc1 (void)
{
  foo_c (c); /* { dg-warning "with different width" "prototype conversion warning" } */
  foo_c (ll); /* { dg-warning "with different width" "prototype conversion warning" } */
  foo_c (f); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_c (ld); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_c (cd); /* { dg-warning "as integer rather than complex" "prototype conversion warning" } */

  foo_ll (c); /* { dg-warning "with different width" "prototype conversion warning" } */
  foo_ll (ll);
  foo_ll (f); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_ll (ld); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_ll (cd); /* { dg-warning "as integer rather than complex" "prototype conversion warning" } */

  foo_f (c); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_f (ll); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_f (f); /* { dg-warning "as `float' rather than `double'" "prototype conversion warning" } */
  foo_f (ld); /* { dg-warning "as `float' rather than `double'" "prototype conversion warning" } */
  foo_f (cd); /* { dg-warning "as floating rather than complex" "prototype conversion warning" } */

  foo_ld (c); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_ld (ll); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_ld (f);
  foo_ld (ld);
  foo_ld (cd); /* { dg-warning "as floating rather than complex" "prototype conversion warning" } */

  foo_cd (c); /* { dg-warning "as complex rather than integer" "prototype conversion warning" } */
  foo_cd (ll); /* { dg-warning "as complex rather than integer" "prototype conversion warning" } */
  foo_cd (f); /* { dg-warning "as complex rather than floating" "prototype conversion warning" } */
  foo_cd (ld); /* { dg-warning "as complex rather than floating" "prototype conversion warning" } */
  foo_cd (cd);
}
  
# 54 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

void
testfunc2 (void)
{
  foo_c (c);
  foo_c (ll);
  foo_c (f);
  foo_c (ld);
  foo_c (cd);

  foo_ll (c);
  foo_ll (ll);
  foo_ll (f);
  foo_ll (ld);
  foo_ll (cd);

  foo_f (c);
  foo_f (ll);
  foo_f (f);
  foo_f (ld);
  foo_f (cd);

  foo_ld (c);
  foo_ld (ll);
  foo_ld (f);
  foo_ld (ld);
  foo_ld (cd);

  foo_cd (c);
  foo_cd (ll);
  foo_cd (f);
  foo_cd (ld);
  foo_cd (cd);
}
