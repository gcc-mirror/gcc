/* Testing save/restore of floating point caller-save registers, on ia64
   this resulted in bad code.  Not all targets will use caller-save regs.  */

/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -minline-float-divide-max-throughput" { target ia64-*-* } } */

/* Testing save/restore of floating point caller-save registers on ia64.  */

extern void abort (void);

double foo(double a, double b, double c)
{
	return (a+b+c);
}

main ()
{
	double  f1, f2, f3, f4, f5, f6, f7, f8, f9,f10;
	double f11,f12,f13,f14,f15,f16,f17,f18,f19,f20;
	double f21,f22,f23,f24,f25,f26,f27,f28,f29,f30;
	double x;
	int i,j,k;

	f1  = 0.1; f2  = 0.2; f3  = 0.3; f4  = 0.4; f5  = 0.5;
	f6  = 0.6; f7  = 0.7; f8  = 0.8; f9  = 0.9; f10 = 1.0;
	f11 = 1.1; f12 = 1.2; f13 = 1.3; f14 = 1.4; f15 = 1.5;
	f16 = 1.6; f17 = 1.7; f18 = 1.8; f19 = 1.9; f20 = 2.0;
	f21 = 2.1; f22 = 2.2; f23 = 2.3; f24 = 2.4; f25 = 2.5;
	f26 = 2.6; f27 = 2.7; f28 = 2.8; f29 = 2.9; f30 = 3.0;

	i = (int) foo(1.0,1.0,1.0);
	while (i > 0) {
		f1  = f2  / f3  * f30;
		f2  = f3  / f4  * f30;
		f3  = f4  / f5  * f30;
		f4  = f5  / f6  * f30;
		f5  = f6  / f7  * f30;
		f6  = f7  / f8  * f30;
		f7  = f8  / f9  * f30;
		f8  = f9  / f10 * f30;
		f9  = f10 / f11 * f30;
		f10 = f11 / f12 * f30;
		f11 = f12 / f13 * f30;
		f12 = f13 / f14 * f25;
		f13 = f14 / f15 * f30;
		f14 = f15 / f16 * f30;
		f15 = f16 / f17 * f30;
		f16 = f17 / f18 * f30;
		f17 = f18 / f19 * f30;
		f18 = f19 / f20 * f30;
		f19 = f20 / f21 * f30;
		f20 = f21 / f22 * f20;
		f21 = f22 / f23 * f30;
		f22 = f23 / f24 * f30;
		f23 = f24 / f25 * f30;
		f24 = f25 / f26 * f30;
		f25 = f26 / f27 * f30;
		f26 = f27 / f28 * f30;
		f27 = f28 / f29 * f30;
		f28 = f29 / f30 * f30;
		f29 = f30 / f1  * f30;
		f30 = f1  / f2  * f30;
		x = foo(f1,f2,f3);
		i = i - 1;
	}
	x = (f1+f2+f3+f4+f5+f6+f7+f8+f9+f10) *
            (f11+f12+f13+f14+f15+f16+f17+f18+f19+f20) *
            (f21+f22+f23+f24+f25+f26+f27+f28+f29+f30);

	/* Exact value is not needed, on IA64 it is massively off. */
        if (x < 19503.0 || x > 19504.0) abort();
	return 0;
}
