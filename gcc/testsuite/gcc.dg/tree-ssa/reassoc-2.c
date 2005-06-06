/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-reassoc-details" } */
extern int a0, a1, a2, a3, a4; 
int f () 
{ 
int b0, b1, b2, b3, b4; 
  /* this can be optimized to four additions... */ 
  b4 = a4 + a3 + a2 + a1 + a0; 
  b3 = a3 + a2 + a1 + a0; 
  b2 = a2 + a1 + a0; 
  b1 = a1 + a0; 
  /* This is actually 0 */
  return b4 - b3 + b2 - b1 - a4 - a2;
} 
/* { dg-final { scan-tree-dump-times "Reassociating by rank" 3 "reassoc" } } */
/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
/* { dg-final { cleanup-tree-dump "reassoc" } } */
