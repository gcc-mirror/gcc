/* { dg-do compile } */
/* { dg-options "-std=c11" } */
/* Origin: Martin Uecker <uecker@eecs.berkeley.edu> */
void tvoid(void* x);
void transpose0(double* out, _Atomic double* in) { }
void transpose1(double out[2][2], _Atomic double in[2][2]) { }
void transpose2(double out[2][2][2], _Atomic double in[2][2][2]) { }
// return
int (*x2(_Atomic int x[3][3]))[3] { return x; } /* { dg-warning "return from incompatible pointer type" } */
_Atomic int (*x3(int x[3][3]))[3] { return x; } /* { dg-warning "return from incompatible pointer type" } */
void test(void)
{
	double x0[2];
	double y0[2];
	_Atomic double z0[4];
	double x1[2][2];
	double y1[2][2];
	double o1[2][3];
	_Atomic double z1[2][2];
	double x2[2][2][2];
	double y2[2][2][2];
	double o2[2][2][3];
	_Atomic double z2[2][2][2];
	tvoid(z0);
	tvoid(z1);
	tvoid(z2);
	// passing as arguments
	transpose0(y0, x0); /* { dg-warning "passing argument 2 of 'transpose0' from incompatible pointer type" } */
	transpose1(y1, o1); /* { dg-warning "passing argument 2 of 'transpose1' from incompatible pointer type" } */
	transpose1(y1, x1); /* { dg-warning "passing argument 2 of 'transpose1' from incompatible pointer type" } */
	transpose2(y2, o2); /* { dg-warning "passing argument 2 of 'transpose2' from incompatible pointer type" } */
	transpose2(y2, x2); /* { dg-warning "passing argument 2 of 'transpose2' from incompatible pointer type" } */
	// initialization
	_Atomic double (*x0p) = x0; /* { dg-warning "initialization from incompatible pointer type" } */
	_Atomic double (*x1p)[2] = x1; /* { dg-warning "initialization from incompatible pointer type" } */
	_Atomic double (*x2p)[2][2] = x2; /* { dg-warning "initialization from incompatible pointer type" } */
	// assignment
	x0p = x0; /* { dg-warning "assignment from incompatible pointer type" } */
	x1p = x1; /* { dg-warning "assignment from incompatible pointer type" } */
	x2p = x2; /* { dg-warning "assignment from incompatible pointer type" } */
	// subtraction
	&(x0[1]) - &(z0[0]); /* { dg-error "invalid operands to binary" } */
	&(x1[1]) - &(z1[0]); /* { dg-error "invalid operands to binary" } */
	&(x2[1]) - &(z2[0]); /* { dg-error "invalid operands to binary" } */
	// comparison
	x0 == z0; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	x1 == z1; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	x2 == z2; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	x0 > z0; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	x1 > z1; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	x2 > z2; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	x0 < z0; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	x1 < z1; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	x2 < z2; /* { dg-warning "comparison of distinct pointer types lacks a cast" } */
	// conditional expressions
	(void)(1 ? x0 : z0); /* { dg-warning "pointer type mismatch in conditional expression" } */
	(void)(1 ? x1 : z1); /* { dg-warning "pointer type mismatch in conditional expression" } */
	(void)(1 ? x2 : z2); /* { dg-warning "pointer type mismatch in conditional expression" } */
}

