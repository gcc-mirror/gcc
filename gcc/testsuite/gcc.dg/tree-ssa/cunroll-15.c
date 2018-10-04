/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops -fdump-tree-cunroll-optimized -fdump-tree-optimized" } */

int Test(void)
{
  int c = 0;
  const int in[4] = {4,3,4,4};
  for (unsigned i = 0; i < 4; i++) {
      for (unsigned j = 0; j < i; j++) {
	  if (in[i] == in[j])
	    break;
	  else 
	    ++c;
      }
  }
  return c;
}

/* { dg-final { scan-tree-dump-times "optimized:\[^\n\r\]*completely unrolled" 2 "cunroll" } } */
/* When SLP vectorization is enabled the following will fail because DOM
   doesn't know how to deal with the vectorized initializer of in.  */
/* DOM also doesn't know to CSE in[1] with in = *.LC0 thus the list of targets this fails.  */
/* { dg-final { scan-tree-dump "return 1;" "optimized" { xfail { arm*-*-* powerpc-*-* } } } } */
