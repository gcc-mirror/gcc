/* PR target/14201  */
/* Excercise float -> integer in memory patterns.  */
/* { dg-options "-mieee" { target alpha*-*-* } }  */

void f1 (float v,  int *p)			{ *p = v; }
void f2 (float v,  unsigned int*p)		{ *p = v; }
void f3 (float v,  long long *p)		{ *p = v; }
void f4 (float v,  unsigned long long *p)	{ *p = v; }
void f5 (double v, int *p)			{ *p = v; }
void f6 (double v, unsigned int *p)		{ *p = v; }
void f7 (double v, long long *p)		{ *p = v; }
void f8 (double v, unsigned long long *p)	{ *p = v; }
