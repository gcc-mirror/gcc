/* Test for -Wtraditional warnings on switch operands of type long.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

void
testfunc (l)
     long l;
{
  switch (l) /* { dg-warning "switch expression" "switch expression" } */
  {
  default:
    break;
  }

# 17 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  switch (l)
  {
  default:
    break;
  }
}
