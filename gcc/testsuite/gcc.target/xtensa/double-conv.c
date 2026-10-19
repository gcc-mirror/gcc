/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */

void si(int v);
void ui(unsigned int v);
void f(float v);
void d(double v);

void dsi(double v)
{
	si(v);
}

void dui(double v)
{
	ui(v);
}

void sid(int v)
{
	d(v);
}

void uid(unsigned int v)
{
	d(v);
}

void fd(float v)
{
	d(v);
}

void df(double v)
{
	f(v);
}

/* { dg-final { scan-assembler-times "trunc.d\t" 2 } } */
/* { dg-final { scan-assembler-times "utrunc.d\t" 1 } } */
/* { dg-final { scan-assembler-times "float.d\t" 2 } } */
/* { dg-final { scan-assembler-times "ufloat.d\t" 1 } } */
/* { dg-final { scan-assembler-times "cvts.d\t" 1 } } */
/* { dg-final { scan-assembler-times "cvtd.s\t" 1 } } */
