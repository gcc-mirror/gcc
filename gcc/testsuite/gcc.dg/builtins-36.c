/* Copyright (C) 2004 Free Software Foundation.

   Check sin, sinf, sinl, cos, cosf and cosl built-in functions
   eventually compile to sincos, sincosf and sincosl.

   Written by Uros Bizjak, 5th April 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double sin(double);
extern float sinf(float);
extern long double sinl(long double);

extern double cos(double);
extern float cosf(float);
extern long double cosl(long double);


double test1(double x)
{
	double y1, y2;

	y1 = sin(x);
	y2 = cos(x);

	return y1 - y2;
}

float test1f(float x)
{
	float y1, y2;

	y1 = sinf(x);
	y2 = cosf(x);

	return y1 - y2;
}

long double test1l(long double x)
{
	long double y1, y2;

	y1 = sinl(x);
	y2 = cosl(x);

	return y1 - y2;
}

double test2(double x)
{
	return sin(x);
}

float test2f(float x)
{
	return sinf(x);
}

long double test2l(long double x)
{
	return sinl(x);
}

double test3(double x)
{
	return cos(x);
}

float test3f(float x)
{
	return cosf(x);
}

long double test3l(long double x)
{
	return cosl(x);
}

