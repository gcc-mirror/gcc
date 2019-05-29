/* Verify whether math functions are simplified.  */
/* { dg-require-effective-target c99_runtime } */
/* { dg-require-weak "" } */
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
main()
{
#ifdef __OPTIMIZE__
	if (t(0)!=0)
		abort ();
	if (q(0)!=0)
		abort ();
	if (q1(0)!=0)
		abort ();
#endif
	return 0;
}
__attribute__ ((weak))
double
floor(double a)
{
	abort ();
}
__attribute__ ((weak))
float
floorf(float a)
{
	return a;
}
__attribute__ ((weak))
double
sin(double a)
{
	return a;
}
__attribute__ ((weak))
float
sinf(float a)
{
	abort ();
}
