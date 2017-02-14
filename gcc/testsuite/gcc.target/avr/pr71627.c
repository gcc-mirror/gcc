/* { dg-do compile } */
/* { dg-options "-O1" } */


extern volatile __memx const long  a, b, c, d, e, f;
extern volatile long result;

extern void vfunc (const char*, ...);

void foo (void)
{
	result = a + b + c + d + e + f;
	vfunc ("text", a, b, c, d, e, f, result);
}
