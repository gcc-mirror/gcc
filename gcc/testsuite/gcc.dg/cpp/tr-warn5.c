/* Test for -Wtraditional warnings on the unary plus operator.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do preprocess } */
/* { dg-options "-Wtraditional -fno-show-column" } */

#if +1 /* { dg-warning "unary plus operator" "unary plus operator" } */
#endif
  
# 11 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

#if +1
#endif
