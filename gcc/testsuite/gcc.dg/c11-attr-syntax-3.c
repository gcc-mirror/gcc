/* Test C23 attribute syntax: rejected in C11, but warning disabled
   with -Wno-c11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wno-c11-c23-compat" } */

[[]];

void f [[]] (void);
