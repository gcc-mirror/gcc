/* PR tree-optimization/14627 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */


int b;
void foo (int a) {
	if (a)
		a = 3;
	b = a;
}

/* Make sure we do not have an assignment a = 0 in the resulting
   optimized dump.  */
/* { dg-final { scan-tree-dump-times "a.* = 0;" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
