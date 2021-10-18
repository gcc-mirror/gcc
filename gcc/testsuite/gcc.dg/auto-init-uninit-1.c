/* Spurious uninitialized variable warnings, case 1.
   Taken from cppfiles.c (merge_include_chains) */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */
#include "uninit-1.c"
