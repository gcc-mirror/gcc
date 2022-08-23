/* This was a missed optimization in tree constant propagation
   that CSE would catch later on.  */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dce3" } */

double _Complex *a; 
static const double _Complex b[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}; 
 
void 
test (void) 
{ 
  a[0] = b[0] + b[1]; 
  a[1] = b[0] + b[1]; 
  return; 
} 

/* After DCE2 which runs after FRE, the expressions should be fully
   constant folded.  There should be no loads from b left.  */
/* { dg-final { scan-tree-dump-times {(?n)REALPART_EXPR.*= 1\.0e\+0} 2 "dce3" } } */
/* { dg-final { scan-tree-dump-times {(?n)IMAGPART_EXPR.*= 0\.0} 2 "dce3" } } */
/* { dg-final { scan-tree-dump-times "= b" 0 "dce3" } } */
