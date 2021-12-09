/* { dg-do compile } */
/* { dg-require-effective-target rop_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect" } */

/* Verify that __ROP_PROTECT__ is predefined for -mrop-protect.  */

int foo ()
{
#ifndef __ROP_PROTECT__
  __ROP_PROTECT__ macro is not defined when it should be
#endif
  return 0;
}

