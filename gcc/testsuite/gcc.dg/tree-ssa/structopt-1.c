/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */
int x; int y;
struct { int x; int y; } global;
int foo() {
	int i;
	for ( i=0; i<10; i++)
		y += x*x;
	for ( i=0; i<10; i++)
		global.y += global.x*global.x;
}

/* { dg-final { scan-tree-dump-times "Executing store motion of global.y" 1 "lim2" } } */
/* XXX: We should also check for the load motion of global.x, but there is no easy way to do this.  */
