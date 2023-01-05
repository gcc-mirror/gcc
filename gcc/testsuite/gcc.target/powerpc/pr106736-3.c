/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* If the default cpu type is power10 or later, type __vector_quad is
   supported.  To keep the test point available all the time, this case
   specifies -mdejagnu-cpu=power9 here.  */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* Verify there is no ICE and don't check the error messages on unsupported
   type since they could be fragile and are not test points of this case.  */

/* { dg-excess-errors "pr106736-3" } */

__vector_quad ga;
void
foo (__vector_quad *a)
{
  ga = *a;
}

