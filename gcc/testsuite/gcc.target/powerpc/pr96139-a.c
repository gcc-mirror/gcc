/* { dg-do compile } */
/* { dg-options "-O2 -Wall -mvsx" } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stdio.h>
#include <altivec.h>

void
try_printing_longlong_a (
                        __vector signed char cval,
                        __vector signed int ival,
                        __vector signed long long int llval,
                        int x, int y, int z)
{
  printf (" %016llx \n", llval[x]);
  printf (" %016x \n", ival[z]);
  printf (" %c \n", cval[y]);
}

void
try_printing_unsigned_longlong_a (
                        __vector unsigned char cval,
                        __vector unsigned int ival,
                        __vector unsigned long long int llval,
                        int x, int y, int z)
{
  printf (" %016llx \n", llval[x]);
  printf (" %016x \n", ival[z]);
  printf (" %c \n", cval[y]);
}

