/* Copyright (C) 2006 Free Software Foundation, Inc. */
/* Contributed by Carlos O'Donell on 2006-01-30 */

/* Test a division corner case where the expression simplifies
   to a comparison, and the optab expansion is wrong. The optab 
   expansion emits a function whose return is unbiased and needs
   adjustment. */
/* Origin: Carlos O'Donell <carlos@codesourcery.com> */
/* { dg-do run { target arm-*-*eabi* } } */
/* { dg-options "" } */
#include <stdlib.h>

#define BIG_CONSTANT 0xFFFFFFFF80000000ULL

int main (void)
{
  unsigned long long OneULL = 1ULL;
  unsigned long long result;

  result = OneULL / BIG_CONSTANT; 
  if (result)
    abort ();
  exit (0);
}
