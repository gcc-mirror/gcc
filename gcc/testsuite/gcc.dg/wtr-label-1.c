/* Test for -Wtraditional warnings on label conflicts with identifiers.
   Note, gcc should omit these warnings in system header files.
   Origin: Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/24/2000.  */
/* { dg-do compile } */
/* { dg-options "-Wtraditional" } */

enum foo1 { a };
struct foo2 { int i; };
union foo3 { int j; };
int foo4;
typedef int foo5;

void
testfunc1 (foo6)
     int foo6;
{
  int foo7;

 foo1:
 foo2:
 foo3:
 foo4: /* { dg-warning "traditional C lacks" "label conflicts with identifier" } */
 foo5: /* { dg-warning "traditional C lacks" "label conflicts with identifier" } */
 foo6: /* { dg-warning "traditional C lacks" "label conflicts with identifier" } */
 foo7: /* { dg-warning "traditional C lacks" "label conflicts with identifier" } */
 testfunc1: /* { dg-warning "traditional C lacks" "label conflicts with identifier" } */
 a: /* { dg-warning "traditional C lacks" "label conflicts with identifier" } */
 i:
 j: ;
}
  
# 32 "sys-header.h" 3
/* We are in system headers now, no -Wtraditional warnings should issue.  */

void
testfunc2 (foo6)
     int foo6;
{
  int foo7;

 foo1:
 foo2:
 foo3:
 foo4:
 foo5:
 foo6:
 foo7:
 testfunc2:
 a:
 i:
 j: ;
}
