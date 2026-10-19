/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */

void si(int v);
void ui(unsigned int v);
void f(float v);

void fsi(float v)
{
	si(v);
}

void fui(float v)
{
	ui(v);
}

void sif(int v)
{
	f(v);
}

void uif(unsigned int v)
{
	f(v);
}

/* { dg-final { scan-assembler-times "trunc.s\t" 2 } } */
/* { dg-final { scan-assembler-times "utrunc.s\t" 1 } } */
/* { dg-final { scan-assembler-times "float.s\t" 2 } } */
/* { dg-final { scan-assembler-times "ufloat.s\t" 1 } } */
