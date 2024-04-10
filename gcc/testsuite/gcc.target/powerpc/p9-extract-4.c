/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

/* This file tests the extraction of 64-bit values.  On Power 9, the direct
   move is prefered for the  64-bit extract as it is either lower latency or
   the same latency as the extract instruction depending on the Endianess of
   the system.  Furthermore, there can be up to four move instructions in
   flight at a time versus only two extract intructions at a time.  */

#include <altivec.h>

unsigned long long
extract_bool_long_long_0 (vector bool long long a)
{
  int c = 0;
  unsigned long long b = vec_extract (a, c);
  return b;
}

unsigned long long int
extract_long_long_0 (vector unsigned long long int a)
{
  int c = 0;
  unsigned long long int b = vec_extract (a, c);
  return b;
}

/* { dg-final { scan-assembler-times "m\[ft\]vsr" 2 } } */
