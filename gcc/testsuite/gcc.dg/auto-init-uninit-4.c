/* Spurious uninit variable warnings, case 4.
   Simplified version of cppexp.c (cpp_parse_expr).

   This one is really fragile, it gets it right if you take out case
   1, or if the structure is replaced by an int, or if the structure
   has fewer members (!) */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */
#include "uninit-4.c"
