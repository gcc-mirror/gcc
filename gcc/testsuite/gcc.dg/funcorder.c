/* { dg-do compile } */
/* { dg-options "-O2 -funit-at-a-time" } */
/* { dg-final { if [ istarget hppa*-*-* ] { scan-assembler-not "link_error,%r" { xfail hppa*64*-*-* } } else { scan-assembler-not "link_error" } } } */
/*  In unit-at-time the functions should be assembled in order
    e q t main, so we realize that they are pure.  The test is
    xfailed on hppa64 because variable r in q is sign extended
    to 64-bits.  As a result, "if (t!=mem)" is not simplified.  */
 
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
