/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple -fwrapv" } */
extern float fabsf (float);
extern float cabsf (_Complex float);

int f (float a) {
	return fabsf(a) < 0.0;
}

int g (_Complex float a) {
	return cabsf (a) < 0.0;
}

/* { dg-final { scan-tree-dump-times "ABS" 0 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
