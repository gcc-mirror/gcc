/* { dg-do run } */
/* { dg-xfail-if "not implemented" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -fzero-call-used-regs=all-arg" } */

#include "zero-scratch-regs-1.c"
