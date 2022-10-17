/* { dg-do compile } */
/* { dg-options "-fwrapv -O1 -ftree-vrp -fdisable-tree-evrp -fdump-tree-vrp1 -fdisable-tree-ethread -fdisable-tree-thread1" } */

#include <limits.h>
extern void abort ();
extern void exit (int);

int f (int a) {
	if (a != INT_MIN) {
		a = a > 0 ? a : -a;
		if (a < 0)
		  return 1;
	}
	return 0;
}

int g (int b) {
	if (b != INT_MIN) {
		b = b > 0 ? b : -b;
		if (b >= 0)
		  return 0;
	}
	return 1;
}
/* { dg-final { scan-tree-dump "Folding predicate a_. < 0 to 0" "vrp1" } } */
/* { dg-final { scan-tree-dump "Folding predicate b_. >= 0 to 1" "vrp1" } } */
