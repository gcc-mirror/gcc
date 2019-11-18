/* Test C2x attribute syntax: rejected in C11, but warning disabled
   with -Wno-c11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wno-c11-c2x-compat" } */

[[]];

void f [[]] (void);
