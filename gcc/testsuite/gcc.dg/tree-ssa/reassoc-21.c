/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int f (unsigned int a0, unsigned int a1, unsigned int a2,
		unsigned int a3, unsigned int a4) 
{ 
  unsigned int b0, b1, b2, b3, b4, e; 
  /* this can be optimized to four additions... */ 
  b4 = a4 + a3 + a2 + a1 + a0; 
  b3 = a3 + a2 + a1 + a0; 
  b2 = a2 + a1 + a0; 
  b1 = a1 + a0; 
  /* This is actually 0 */
  e = b4 - b3 + b2 - b1 - a4 - a2;
  return e;
}

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
