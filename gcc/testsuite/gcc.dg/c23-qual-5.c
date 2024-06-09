/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */
void tvoid(void* x);
void transpose0(double* out, const double* in) { }
void transpose1(double out[2][2], const double in[2][2]) { }
void transpose2(double out[2][2][2], const double in[2][2][2]) { }
// return
int (*x2(const int x[3][3]))[3] { return x; } /* { dg-error "return discards" } */
const int (*x3(int x[3][3]))[3] { return x; }
void test(void)
{
	double x0[2];
	double y0[2];
	const double z0[4];
	double x1[2][2];
	double y1[2][2];
	double o1[2][3];
	const double z1[2][2];
	double x2[2][2][2];
	double y2[2][2][2];
	double o2[2][2][3];
	const double z2[2][2][2];
	// void pointers
	tvoid(z0); /* { dg-error "passing argument 1 of 'tvoid' discards 'const' qualifier from pointer target type" } */
	tvoid(z1); /* { dg-error "passing argument 1 of 'tvoid' discards 'const' qualifier from pointer target type" } */
	tvoid(z2); /* { dg-error "passing argument 1 of 'tvoid' discards 'const' qualifier from pointer target type" } */
	void* p;
	const void* pc;
	p = x0;
	p = x1;
	p = x2;
	p = z0; /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	p = z1; /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	p = z2; /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	pc = x0;
	pc = x1;
	pc = x2;
	pc = z0;
	pc = z1;
	pc = z2;
	transpose0(pc, p); /* { dg-error "passing argument 1 of 'transpose0' discards 'const' qualifier from pointer target type" } */
	transpose1(pc, p); /* { dg-error "passing argument 1 of 'transpose1' discards 'const' qualifier from pointer target type" } */
	transpose2(pc, p); /* { dg-error "passing argument 1 of 'transpose2' discards 'const' qualifier from pointer target type" } */
	transpose0(p, pc);
	transpose1(p, pc);
	transpose2(p, pc);
	// passing as arguments
	transpose0(y0, x0);
	transpose1(y1, o1); /* { dg-error "passing argument 2 of 'transpose1' from incompatible pointer type" } */
	transpose1(y1, x1);
	transpose2(y2, o2); /* { dg-error "passing argument 2 of 'transpose2' from incompatible pointer type" } */
	transpose2(y2, x2);
	// initialization
	const double (*x0p) = x0;
	const double (*x1p)[2] = x1;
	const double (*x2p)[2][2] = x2;
	double (*v0p) = z0; /* { dg-error "initialization discards 'const' qualifier from pointer target type" } */
	double (*v1p)[2] = z1; /* { dg-error "initialization discards" } */
	double (*v2p)[2][2] = z2; /* { dg-error "initialization discards" } */
	// assignment
	x0p = x0;
	x1p = x1;
	x2p = x2;
	// subtraction
	&(x0[1]) - &(z0[0]);
	&(x1[1]) - &(z1[0]);
	&(x2[1]) - &(z2[0]);
	// comparison
	x0 == z0;
	x1 == z1;
	x2 == z2;
	x0 < z0;
	x1 < z1;
	x2 < z2;
	x0 > z0;
	x1 > z1;
	x2 > z2;
	// conditional expressions
	(void)(1 ? x0 : z0);
	(void)(1 ? x1 : z1);
	(void)(1 ? x2 : z2);
	(void)(1 ? x0 : x1); /* { dg-error "pointer type mismatch in conditional expression" } */
	(void)(1 ? x1 : x2); /* { dg-error "pointer type mismatch in conditional expression" } */
	(void)(1 ? x2 : x0); /* { dg-error "pointer type mismatch in conditional expression" } */
	v0p = (1 ? z0 : v0p); /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	v1p = (1 ? z1 : v1p); /* { dg-error "assignment discards" } */
	v2p = (1 ? z2 : v2p); /* { dg-error "assignment discards" } */
	v0p = (1 ? x0 : x0p); /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	v1p = (1 ? x1 : x1p); /* { dg-error "assignment discards" } */
	v2p = (1 ? x2 : x2p); /* { dg-error "assignment discards" } */
	(1 ? x0 : z0)[0] = 1; /* { dg-error "assignment of read-only location" } */
	(1 ? x1 : z1)[0][0] = 1; /* { dg-error "assignment of read-only location" } */
	(1 ? x2 : z2)[0][0][0] = 1; /* { dg-error "assignment of read-only location" } */
	v0p = (1 ? p : z0); /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	v1p = (1 ? p : z1); /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	v2p = (1 ? p : z2); /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	v0p = (1 ? pc : x0); /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	v1p = (1 ? pc : x1); /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
	v2p = (1 ? pc : x2); /* { dg-error "assignment discards 'const' qualifier from pointer target type" } */
}

