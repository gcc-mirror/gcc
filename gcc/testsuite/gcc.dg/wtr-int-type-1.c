/* Test for -Wtraditional warnings on integer constant types.
   Note, gcc should omit these warnings in system header files.
   By Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/22/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

void
testfunc (void)
{
  long long i;
  
  i = 0x80000000;
  i = 0x8000000000000000;
  i = 9223372036854775807; /* { dg-warning "integer constant.*with -traditional" "integer constant" } */
  i = 4294967295; /* { dg-warning "decimal constant|integer constant.*with -traditional" "integer constant" } */
  
#line 18 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

  i = 0x80000000;
  i = 0x8000000000000000;
  i = 9223372036854775807;
  i = 4294967295; /* { dg-warning "decimal constant" "decimal constant" } */
}
