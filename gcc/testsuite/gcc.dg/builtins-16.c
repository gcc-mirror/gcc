/* Copyright (C) 2003  Free Software Foundation.

   Verify that all the __builtin_cabs? functions are recognized
   by the compiler.  Complex numbers are not supported with the
   gcc.dg default "-pedantic-errors" option, so the dg-options
   overrides this.

   Written by Roger Sayle, 1st June 2003.  */

/* { dg-do compile } */
/* { dg-options "-O -ansi" } */
/* { dg-final { scan-assembler-not "__builtin_" } } */

double test(__complex__ double x)
{
  return __builtin_cabs (x);
}

float testf(__complex__ float x)
{
  return __builtin_cabsf (x);
}

long double testl(__complex__ long double x)
{
  return __builtin_cabsl (x);
}

