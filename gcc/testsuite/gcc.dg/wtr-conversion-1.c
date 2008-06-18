/* Test for -Wtraditional warnings on conversions by prototypes.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 4/09/2001.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

extern void foo_i (int);
extern void foo_f (float);
extern void foo_ld (long double);
extern void foo_cd (__complex__ double);

extern int i;
extern float f;
extern long double ld;
extern __complex__ double cd;

void
testfunc1 ()
{
  foo_i (i);
  foo_i (f); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_i (ld); /* { dg-warning "as integer rather than floating" "prototype conversion warning" } */
  foo_i (cd); /* { dg-warning "as integer rather than complex" "prototype conversion warning" } */

  foo_f (i); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_f (f); /* { dg-warning "as 'float' rather than 'double'" "prototype conversion warning" } */
  foo_f (ld); /* { dg-warning "as 'float' rather than 'double'" "prototype conversion warning" } */
  foo_f (cd); /* { dg-warning "as floating rather than complex" "prototype conversion warning" } */

  foo_ld (i); /* { dg-warning "as floating rather than integer" "prototype conversion warning" } */
  foo_ld (f); /* { dg-warning "as 'float' rather than 'double'" "small double" { target { "avr-*-*" } } } */
  foo_ld (ld);/* { dg-warning "as 'float' rather than 'double'" "small long double" { target { "avr-*-*" } } } */
  foo_ld (cd);/* { dg-warning "as floating rather than complex" "prototype conversion warning" } */

  foo_cd (i); /* { dg-warning "as complex rather than integer" "prototype conversion warning" } */
  foo_cd (f); /* { dg-warning "as complex rather than floating" "prototype conversion warning" } */
  foo_cd (ld); /* { dg-warning "as complex rather than floating" "prototype conversion warning" } */
  foo_cd (cd);
}
  
# 54 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

void
testfunc2 ()
{
  foo_i (i);
  foo_i (f);
  foo_i (ld);
  foo_i (cd);

  foo_f (i);
  foo_f (f);
  foo_f (ld);
  foo_f (cd);

  foo_ld (i);
  foo_ld (f);
  foo_ld (ld);
  foo_ld (cd);

  foo_cd (i);
  foo_cd (f);
  foo_cd (ld);
  foo_cd (cd);
}
