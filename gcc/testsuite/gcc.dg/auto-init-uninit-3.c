/* Spurious uninit variable warnings, case 3.
   Inspired by cppexp.c (parse_charconst) */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */
#include "uninit-3.c"
