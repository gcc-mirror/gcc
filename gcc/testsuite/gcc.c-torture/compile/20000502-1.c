static int minimum(int a, int b)
{
	if(a < b)
		return a;
	else
		return b;
}
static int a, b;
static inline int foo(void)
{
	a = minimum (a, b);
	return 0;
}
static int bar(void)
{
	return foo();
}
