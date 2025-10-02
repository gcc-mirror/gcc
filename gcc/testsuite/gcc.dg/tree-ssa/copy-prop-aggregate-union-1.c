/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details" } */
/* PR tree-optimization/107051 */


union U2 {
   unsigned  f0;
   char * f1;
};

/* Since g_284[0] and g_284[1] are known not overlap,
   copy prop can happen.  */
union U2 g_284[2] = {{0UL},{0xC2488F72L}};

int e;
void func_1() {
	union U2 c = {7};
	int *d[2];
	for (; e;)
		*d[1] = 0;
	g_284[0] = c = g_284[1];
}

/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */
