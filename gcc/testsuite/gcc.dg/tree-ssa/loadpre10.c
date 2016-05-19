/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats -fno-tree-loop-im" } */
struct tree_common 
{ 
  int code; 
}; 
union tree_node 
{ 
  struct tree_common common; 
}; 
typedef union tree_node *tree; 
 
extern tree test (tree, int, int); 
extern tree foo (void); 
extern void abort (void) __attribute__ ((__noreturn__)); 

/* Redundant loads of expr->common.code */
tree 
test (tree expr, int t, int D17630) 
{ 
  int __i; 
 
L0: 
  if (expr->common.code != 142) goto L23; else goto L2; 
 
L2: 
  __i = 0; 
  goto L10; 
 
L10: 
  __i = __i + 1; 
  if (D17630 != __i) goto L8; else goto L19; 
 
L8: 
  if (t) goto L15; else goto L10; 
 
L15: 
  expr = foo (); 
  if (expr->common.code != 142) goto L23; else goto L0; 
 
L19: 
  abort (); 
 
L23: 
  return expr; 
} 
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */

