/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread4-details" } */

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


/* We should thread the backedge to the top of the loop; ie we only
   execute the if (expr->common.code != 142) test once per loop
   iteration.  */
/* { dg-final { scan-tree-dump-times "FSM jump thread" 1 "thread4" } } */

