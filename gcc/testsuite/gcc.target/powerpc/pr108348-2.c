/* { dg-options "-mdejagnu-cpu=power9 -mabi=no-altivec" } */
/* { dg-require-effective-target powerpc_altivec } */
/* If the default cpu type is power10 or later, type __vector_pair is
   supported.  To keep the test point available all the time, this case
   specifies -mdejagnu-cpu=power9 here.  This needs -mabi=no-altivec
   to do the copying for pass-by-reference function argument on 32 bit
   environment.  */

/* Verify there is no ICE on 32 bit and don't check the error messages
   on unsupported type since they could be fragile and are not test
   points of this case.  */

/* { dg-excess-errors "pr108348-2" } */

extern void bar (__vector_pair v);

void
foo (void)
{
  __vector_pair v;
  bar (v);
}

