/* Spurious uninitialized variable warnings.  Slight variant on the
   documented case, inspired by reg-stack.c:record_asm_reg_life.  */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */

#include "uninit-9.c"
