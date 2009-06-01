/* Test for -Wtraditional warnings on integer constant suffixes.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do preprocess } */
/* { dg-options "-Wtraditional" } */

#if 1U /* { dg-warning "traditional C rejects" "numeric constant suffix" } */
#endif
#if 1u /* { dg-warning "traditional C rejects" "numeric constant suffix" } */
#endif
#if 1L
#endif
#if 1l
#endif
  
# 17 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

#if 1U
#endif
#if 1u
#endif
#if 1L
#endif
#if 1l
#endif
