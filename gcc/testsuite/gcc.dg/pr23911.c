/* This was a missed optimization in tree constant propagation
   that CSE would catch later on.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-store_ccp" } */

double _Complex *a; 
static const double _Complex b[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}; 
 
void 
test (void) 
{ 
  a[0] = b[0] + b[1]; 
  a[1] = b[0] + b[1]; 
  return; 
} 

/* After store_ccp, there should not be any assignments from real or
   imaginary parts anymore.  The constants should be loaded from b and
   propagated into the elements of a.  */
/* { dg-final { scan-tree-dump-times "= CR" 0 "store_ccp" } } */
/* { dg-final { scan-tree-dump-times "= CI" 0 "store_ccp" } } */
/* { dg-final { cleanup-tree-dump "store_ccp" } } */
