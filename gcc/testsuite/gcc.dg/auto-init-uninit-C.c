/* Spurious uninitialized variable warning, inspired by libgcc2.c.  */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */

#include "uninit-C.c"
