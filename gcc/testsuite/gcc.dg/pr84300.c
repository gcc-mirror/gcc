/* { dg-do compile } */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-g -O2 -fsplit-stack -fno-omit-frame-pointer" } */

void trap () { __builtin_trap (); }
