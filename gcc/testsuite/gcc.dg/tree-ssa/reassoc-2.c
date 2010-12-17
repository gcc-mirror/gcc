/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f (int a0,int a1,int a2,int a3,int a4) 
{ 
int b0, b1, b2, b3, b4,e; 
  /* this can be optimized to four additions... */ 
  b4 = a4 + a3 + a2 + a1 + a0; 
  b3 = a3 + a2 + a1 + a0; 
  b2 = a2 + a1 + a0; 
  b1 = a1 + a0; 
  /* This is actually 0 */
  e = b4 - b3 + b2 - b1 - a4 - a2;
  return e;
}

/* We can't reassociate the expressions due to undefined signed overflow.  */

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
