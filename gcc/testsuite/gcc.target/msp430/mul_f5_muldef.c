/* { dg-do link } */
/* { dg-options "-mhwmult=f5series" } */

/* This program used to result in a multiple definition error:

libmul_f5.a(lib2hw_mul_f5.o): In function `__mulhi2_f5':
(.text.__mulhi2_f5+0x0): multiple definition of `__mspabi_mpyi'
libgcc.a(mpy.o):mpy.c:(.text.__mulhi3+0x0): first defined here */

#include <stdio.h>

int main (void)
{
  printf ("%d", 430);
}
