/* { dg-do run }
 * { dg-options "-std=c23" }
 */

// bit-fields

struct foo { char (*y)[]; unsigned x:3; } x;

int main()
{
	struct foo { char (*y)[1]; unsigned x:3; } y;

	typeof(*(1 ? &x : &y)) a;
	a.x = 8;			/* { dg-warning "changes value" } */

	if (a.x)
		__builtin_abort();
}


