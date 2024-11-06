/* { dg-do compile } */
/* Origin: Martin Uecker <uecker@eecs.berkeley.edu> */
/* { dg-options "-std=gnu17" } */
void tvoid(void* x);
void transpose0(double* out, const double* in) { }
void transpose1(double out[2][2], const double in[2][2]) { }
void transpose2(double out[2][2][2], const double in[2][2][2]) { }
// return
int (*y2(const int x[3][3]))[3] { return x; } /* { dg-warning "return discards 'const' qualifier from pointer target type" } */
const int (*y3(int x[3][3]))[3] { return x; }
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
	tvoid(x0);
	tvoid(x1);
	tvoid(x2);
	tvoid(z0); /* { dg-warning "passing argument 1 of 'tvoid' discards 'const' qualifier from pointer target type" } */
	tvoid(z1); /* { dg-warning "passing argument 1 of 'tvoid' discards 'const' qualifier from pointer target type" } */
	tvoid(z2); /* { dg-warning "passing argument 1 of 'tvoid' discards 'const' qualifier from pointer target type" } */
	void* p;
	const void* pc;
	p = x0;
	p = x1;
	p = x2;
	p = z0; /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	p = z1; /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	p = z2; /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	pc = x0;
	pc = x1;
	pc = x2;
	pc = z0;
	pc = z1;
	pc = z2;
	transpose0(pc, p); /* { dg-warning "passing argument 1 of 'transpose0' discards 'const' qualifier from pointer target type" } */
	transpose1(pc, p); /* { dg-warning "passing argument 1 of 'transpose1' discards 'const' qualifier from pointer target type" } */
	transpose2(pc, p); /* { dg-warning "passing argument 1 of 'transpose2' discards 'const' qualifier from pointer target type" } */
	transpose0(p, pc);
	transpose1(p, pc);
	transpose2(p, pc);
	// passing as arguments
	transpose0(y0, x0);
	transpose1(y1, x1);
	transpose2(y2, x2);
	// initialization
	const double (*u0p) = x0;
	const double (*u1p)[2] = x1;
	const double (*u2p)[2][2] = x2;
	double (*v0p) = z0; /* { dg-warning "initialization discards 'const' qualifier from pointer target type" } */
	double (*v1p)[2] = z1; /* { dg-warning "initialization discards 'const' qualifier from pointer target type" } */
	double (*v2p)[2][2] = z2; /* { dg-warning "initialization discards 'const' qualifier from pointer target type" } */
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
	// assignment
	u0p = x0;
	u1p = x1;
	u2p = x2;
	v0p = z0; /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v1p = z1; /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v2p = z2; /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	// conditional expressions
	(void)(1 ? x0 : z0);
	(void)(1 ? x1 : z1);
	(void)(1 ? x2 : z2);
	(void)(1 ? x0 : x1); /* { dg-error "pointer type mismatch in conditional expression" } */
	(void)(1 ? x1 : x2); /* { dg-error "pointer type mismatch in conditional expression" } */
	(void)(1 ? x2 : x0); /* { dg-error "pointer type mismatch in conditional expression" } */
	v0p = (1 ? z0 : v0p); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v1p = (1 ? z1 : v1p); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v2p = (1 ? z2 : v2p); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v0p = (1 ? x0 : u0p); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v1p = (1 ? x1 : u1p); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v2p = (1 ? x2 : u2p); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	(1 ? x0 : z0)[0] = 1; /* { dg-error "assignment of read-only location" } */
	(1 ? x1 : z1)[0][0] = 1; /* { dg-error "assignment of read-only location" } */
	(1 ? x2 : z2)[0][0][0] = 1; /* { dg-error "assignment of read-only location" } */
	v0p = (1 ? p : z0); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v1p = (1 ? p : z1); /* { dg-warning "pointer to array loses qualifier in conditional expression" } */
	v2p = (1 ? p : z2); /* { dg-warning "pointer to array loses qualifier in conditional expression" } */
	v0p = (1 ? pc : x0); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v1p = (1 ? pc : x1); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
	v2p = (1 ? pc : x2); /* { dg-warning "assignment discards 'const' qualifier from pointer target type" } */
}

