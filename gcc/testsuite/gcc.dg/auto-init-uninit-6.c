/* Spurious uninitialized variable warnings.
   This one inspired by java/class.c:build_utf8_ref.  */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */

#include "uninit-6.c"
