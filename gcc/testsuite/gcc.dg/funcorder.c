/* { dg-do compile } */
/* { dg-options "-O2 -funit-at-a-time" } */
/* { dg-final { scan-assembler-not "link_error" } } */
/*  In unit-at-time the functions should be assembled in order
    e q t main, so we realize that they are pure.  */
 
static int mem;
static int e(void) __attribute__ ((noinline));
static int q(void) __attribute__ ((noinline));
static int t(void) __attribute__ ((noinline));
main()
{
	return t();
}
static t()
{
	int r,e;
	if (mem)
		t();
	e=mem;
	r=q();
	if (e!=mem)
		link_error();
	return r;
}
static int e()
{
	return 0;
}
static int q()
{
	int t=mem,r;
	r=e();
	if (t!=mem)
		link_error();
	return r;
}
