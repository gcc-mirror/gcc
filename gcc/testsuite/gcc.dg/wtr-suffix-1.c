/* Test for -Wtraditional warnings on integer constant suffixes.
   Note, gcc should omit these warnings in system header files.
   Origin: Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/24/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

void
testfunc (void)
{
  int i;
  double f;

  i = 1L;
  i = 1l;
  i = 1U; /* { dg-warning "traditional C rejects the 'u' suffix" "numeric constant suffix" } */
  i = 1u; /* { dg-warning "traditional C rejects the 'u' suffix" "numeric constant suffix" } */
  f = 1.0;
  f = 1.0F; /* { dg-warning "traditional C rejects the 'f' suffix" "numeric constant suffix" } */
  f = 1.0f; /* { dg-warning "traditional C rejects the 'f' suffix" "numeric constant suffix" } */
  f = 1.0L; /* { dg-warning "traditional C rejects the 'l' suffix" "numeric constant suffix" } */
  f = 1.0l; /* { dg-warning "traditional C rejects the 'l' suffix" "numeric constant suffix" } */
 
# 24 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  i = 1L;
  i = 1l;
  i = 1U;
  i = 1u;
  f = 1.0;
  f = 1.0F;
  f = 1.0f;
  f = 1.0L;
  f = 1.0l;
}
