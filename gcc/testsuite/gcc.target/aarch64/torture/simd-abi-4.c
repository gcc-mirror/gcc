/* { dg-do run } */
/* { dg-require-effective-target aarch64_variant_pcs } */
/* { dg-additional-options "-std=c99" }  */



/* There is nothing special about the calculations here, this is just
   a test that can be compiled and run.  */

extern void abort (void);

__Float64x2_t __attribute__ ((noinline, aarch64_vector_pcs))
foo(__Float64x2_t a, __Float64x2_t b, __Float64x2_t c,
    __Float64x2_t d, __Float64x2_t e, __Float64x2_t f,
    __Float64x2_t g, __Float64x2_t h, __Float64x2_t i)
{
	__Float64x2_t w, x, y, z;
	w = a + b * c;
	x = d + e * f;
	y = g + h * i;
	return w + x * y;
}


int main()
{
	__Float64x2_t a, b, c, d;
	a = (__Float64x2_t) { 1.0, 2.0 };
	b = (__Float64x2_t) { 3.0, 4.0 };
	c = (__Float64x2_t) { 5.0, 6.0 };
	d = foo (a, b, c, (a+b), (b+c), (a+c), (a-b), (b-c), (a-c)) + a + b + c;
	if (d[0] != 337.0 || d[1] != 554.0)
		abort ();
	return 0;
}
