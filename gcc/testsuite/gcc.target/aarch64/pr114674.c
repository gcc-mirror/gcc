/* { dg-do compile } */
/* { dg-options "-O3 --param=aarch64-stp-policy=aligned" } */
typedef struct {
	unsigned int f1;
	unsigned int f2;
} test_struct;

static test_struct ts = {
	123, 456
};

void foo(void)
{
	ts.f2 = 36969 * (ts.f2 & 65535) + (ts.f1 >> 16);
	ts.f1 = 18000 * (ts.f2 & 65535) + (ts.f2 >> 16);
}
/* { dg-final { scan-assembler-times "stp" 1 } } */
