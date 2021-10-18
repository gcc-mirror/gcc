/* Uninitialized variable warning tests...
   Inspired by part of optabs.c:expand_binop.
   May be the same as uninit-1.c.  */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */

#include "uninit-8.c"
