/* Test for -Wtraditional warnings on union initialization.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 9/11/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

union foo1
{
  float f;
  int i;
  long l;
};

union foo2
{
  int i;
  float f;
  long l;
};

void
testfunc ()
{
  /* Note we only warn for nonzero initializers.  */
  static union foo1 f1 = {0};
  static union foo2 f2 = {0};
  static union foo1 f3 = {1}; /* { dg-warning "traditional C rejects initialization of unions" "initialization of unions" } */
  static union foo2 f4 = {1}; /* { dg-warning "traditional C rejects initialization of unions" "initialization of unions" } */
  static union foo1 f5 = {0.0};
  static union foo2 f6 = {0.0};
  static union foo1 f7 = {1.0}; /* { dg-warning "traditional C rejects initialization of unions" "initialization of unions" } */
  static union foo2 f8 = {1.0}; /* { dg-warning "traditional C rejects initialization of unions" "initialization of unions" } */

# 35 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  static union foo1 b1 = {0};
  static union foo2 b2 = {0};
  static union foo1 b3 = {1};
  static union foo2 b4 = {1};
  static union foo1 b5 = {0.0};
  static union foo2 b6 = {0.0};
  static union foo1 b7 = {1.0};
  static union foo2 b8 = {1.0};
}
