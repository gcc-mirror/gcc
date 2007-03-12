/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-fdump-tree-gimple -fstrict-overflow" } */

#define ABS(x) (x > 0 ? x : -x)

unsigned int f (unsigned int a) {
	/* (unsigned)-8 is not a power of 2.  */
	return a % -8;
}

int g (int b) {
	return ABS (b) % -8;
}

int h (int c) {
	return ABS (c) % 8;
}

unsigned int k (unsigned int d) {
	return d % 8;
}

/* { dg-final { scan-tree-dump "a % (4294967288|0fffffff8)" "gimple" } } */
/* { dg-final { scan-tree-dump-times " & 7" 3 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
