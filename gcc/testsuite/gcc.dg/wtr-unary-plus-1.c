/* Test for -Wtraditional warnings on the unary plus operator.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

void
testfunc ()
{
  int i;
  
  i = +1; /* { dg-warning "unary plus operator" "unary plus operator" } */
  i = +i; /* { dg-warning "unary plus operator" "unary plus operator" } */
  
# 16 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  i = +1;
  i = +i;
}
