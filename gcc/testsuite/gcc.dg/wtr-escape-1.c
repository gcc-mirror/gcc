/* Test for -Wtraditional warnings on escape characters.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

void
testfunc (void)
{
  char c;

  c = '\a'; /* { dg-warning "the meaning of" "escaped character warning " } */
  c = '\x2'; /* { dg-warning "the meaning of" "escaped character warning " } */
  c = '\n';

# 17 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  c = '\a';
  c = '\x2';
  c = '\n';
}
