/* PR target/109117 */
/* { dg-do compile } */
/* { dg-options "-mvaes -mno-avx512vl" } */

typedef char __v16qi __attribute__ ((__vector_size__(16)));
typedef long long __m128i __attribute__((__vector_size__(16), __aligned__(16)));
volatile __v16qi x, y;
volatile __m128i res;

void
foo (void)
{
      res = __builtin_ia32_vaesdec_v16qi (x, y); /* { dg-error "incompatible types when assigning to type" } */
}
