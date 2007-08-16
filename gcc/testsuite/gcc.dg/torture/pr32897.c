/* { dg-options "-G0" { target mips*-*-* } } */

volatile int g[32];
long long gll;
double gd;

#define MULTI(X) \
	X( 1), X( 2), X( 3), X( 4), X( 5), X( 6), X( 7), X( 8), X( 9), X(10), \
	X(11), X(12), X(13), X(14), X(15), X(16), X(17), X(18), X(19), X(20), \
	X(21), X(22), X(23), X(24), X(25), X(26), X(27), X(28), X(29), X(30)

#define DECLARE(INDEX) x##INDEX
#define COPY_IN(INDEX) x##INDEX = g[INDEX]
#define COPY_OUT(INDEX) g[INDEX] = x##INDEX

void
test (int n)
{
  union { long long l; double d; } u = { 0x12345678 };
  gll = u.l;
  int MULTI (DECLARE);
  MULTI (COPY_IN);
  MULTI (COPY_OUT);
  MULTI (COPY_OUT);
  MULTI (COPY_OUT);
  gd = u.d;
}
