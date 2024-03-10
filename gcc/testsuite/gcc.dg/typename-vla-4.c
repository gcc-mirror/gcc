/* { dg-do run } 
 * { dg-options "-std=gnu99" }
 * */

int main()
{
	int n = 1;
	sizeof(int[n++]);
	typeof(int[n++]);			/* { dg-warning "empty declaration" } */
	struct { int x[n++]; };			/* { dg-warning "no instance" } */
	struct foo { int x[n++]; };
	struct { int x[n++]; } x;
	struct bar { int x[n++]; } y;
	(int(*)[n++])0;
	(typeof(int(*)[n++]))0;
	(struct { int x[n++]; }*)0;
	(struct q { int x[n++]; }*)0;
	typeof(struct { int x[n++]; });		/* { dg-warning "empty declaration" } */
	typeof(struct r { int x[n++]; });	/* { dg-warning "empty declaration" } */

	if (13 != n)
		__builtin_abort();
}
