/* Test for -Wtraditional warnings on string concatenation.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

void
testfunc (void)
{
  const char *foo;
  
  foo = "hello" "hello"; /* { dg-warning "string concatenation" "string concatenation" } */

# 15 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  foo = "hello" "hello";
}
