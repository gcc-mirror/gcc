/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O0 -mx32" } */

int a;
int* __attribute__ ((ms_abi)) fn1 () { return &a; } /* { dg-error "X32 does not support ms_abi attribute" } */
