/* Verify whether math functions are simplified.  */
double sin(double);
double floor(double);
float 
t(float a)
{
	return sin(a);
}
float 
q(float a)
{
	return floor(a);
}
double
q1(float a)
{
	return floor(a);
}
float
q2(double a)
{
	return floor(a);
}
main()
{
#ifdef __OPTIMIZE__
	if (t(0)!=0)
		abort ();
	if (q(0)!=0)
		abort ();
	if (q1(0)!=0)
		abort ();
	if (q2(0)!=0)
		abort ();
#endif
	return 0;
}
__attribute__ ((noinline))
double
floor(double a)
{
	abort ();
}
__attribute__ ((noinline))
float
floorf(float a)
{
	return a;
}
__attribute__ ((noinline))
double
sin(double a)
{
	abort ();
}
__attribute__ ((noinline))
float
sinf(float a)
{
	return a;
}
