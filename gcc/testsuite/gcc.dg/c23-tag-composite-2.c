/* { dg-do compile } */
/* { dg-options "-std=c23" } */


struct foo { int (*(*i)(void))[]; } x;


void f(void)
{ 
	const struct foo { int (*(*i)())[3]; } y;
	_Static_assert(3 * sizeof(int) == sizeof(*((1 ? &x : &y)->i())), "");
}

void g(struct foo { int x; } a);
void g(const struct foo { int x; } a);

