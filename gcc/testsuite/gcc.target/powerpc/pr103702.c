/* We don't have one powerpc.*_ok for Power6, use altivec_ok conservatively.  */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-mdejagnu-cpu=power6 -O2 -ftree-loop-vectorize -fno-tree-scev-cprop" } */

/* Verify there is no ICE.  */

unsigned short a, e;
int *b, *d;
int c;
extern int fn2 ();
void
fn1 ()
{
  void *f;
  for (;;)
    {
      fn2 ();
      b = f;
      e = 0;
      for (; e < a; ++e)
	b[e] = d[e * c];
    }
}

