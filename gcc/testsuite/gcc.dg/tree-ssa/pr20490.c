/* { dg-do compile } */
/* { dg-options "-O -ftree-pre" } */
static int  a;
static int  b;

typedef int gint;

int blah ()
{
	gint x = a;
	gint y = b;

	x *= (x < 0) ? -1 : 0;
	y *= (y < 0) ? -1 : 0;

	return (y * x);

}


