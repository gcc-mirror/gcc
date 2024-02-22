/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

/* This caused an unrecognizable insn message on development versions of GCC 7.  */

float a;
char b;

void c(void)
{
  a = b = a;
}
