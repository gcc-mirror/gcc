/* Test for -Wtraditional warnings on static/non-static mismatches.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu>  8/22/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

static void testfunc1(void);
void testfunc1() {} /* { dg-warning "non-static.*follows static" "non-static follows static" } */

# 11 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

static void testfunc2(void);
void testfunc2() {}
