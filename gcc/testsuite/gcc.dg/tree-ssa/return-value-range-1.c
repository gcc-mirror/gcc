/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-evrp-details" } */
__attribute__ ((__noinline__))
int a(signed char c)
{
	return c;
}
void link_error ();

void
test(int d)
{
	if (a(d) > 200)
		link_error ();
}
int
main(int argc, char **argv)
{
	test(argc);
	return 0;
}
/* { dg-final { scan-tree-dump-times "Recording return range" 2 "evrp"} } */
