/* Spurious uninitialized variable warnings, case 2.
   Taken from cpphash.c (macroexpand) */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */
#include "uninit-2.c"
