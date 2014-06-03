/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-options "-O -fselective-scheduling -fno-if-conversion -fschedule-insns"  } */

int n;

void
foo (int w, int **dnroot, int **dn)
{
  int *child;
  int *xchild = xchild;
  for (; w < n; w++)
    if (!dnroot)
      {
	dnroot = dn;
	for (child = *dn; child; child = xchild)
	  ;
      }
}
