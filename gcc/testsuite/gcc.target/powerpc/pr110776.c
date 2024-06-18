/* { dg-options "-O2 -mdejagnu-cpu=power6 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

/* Verify there is no ICE.  */

int a;
long *b;
int
c ()
{
  long e;
  int d = 0;
  for (long f; f; f++)
    {
      e = b[f * a];
      if (e)
	d = 1;
    }
  if (d)
    for (;;)
      ;
}
