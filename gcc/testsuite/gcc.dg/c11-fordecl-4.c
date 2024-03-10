/* Test restrictions on the kind of declarations permitted in for loops removed
   in C2X, and thus with -std=c11 -pedantic -Wno-c11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wno-c11-c2x-compat" } */

#include "c99-fordecl-3.c"
