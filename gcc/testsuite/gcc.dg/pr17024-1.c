/* PR middle-end/17024 */
/* { dg-do compile } */
/* { dg-options "-funsafe-math-optimizations" } */

#define MAX2(a,b) (((a)>(b)) ? (a) : (b))

void C(double);

void i(int k)
{
	double c[1];
	C(MAX2(0.,c[k]));
	return;
}

