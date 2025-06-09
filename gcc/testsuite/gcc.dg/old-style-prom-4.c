/* { dg-do compile } */
/* { dg-options "-std=gnu17" } */

int g(float a, float b);
int g(a, b)
	float a;
	float b;
{
}

int f(float a, float (*b)());	/* { dg-error "prototype declaration" } */

int f(a, b)
	float a;
	float (*b)(float);	/* { dg-error "match prototype" } */
{
}

int (*e(float a))(float (*b)());

int (*e(a))(float (*b)(float))		/* { dg-error "conflicting types" } */
	float a;
{
}

