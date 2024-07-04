/* Explicitly disable VSX when VSX is on.  */
/* { dg-options "-mno-vsx" { target powerpc_vsx } } */

/* Verify there is no ICE.  */

long double a;
long double b;

int
foo ()
{
  if (a > b)
    return 0;
  else
    return 1;
}
