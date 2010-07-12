/* PR target/13685 */
/* { dg-options "-Os -msse" } */
/* { dg-require-effective-target sse } */

typedef float __m128 __attribute__ ((vector_size (16)));
typedef int __m64 __attribute__ ((vector_size (8)));

int puts (const char *s);
void foo (__m128 *, __m64 *, int);

int main (void)
{
  foo (0, 0, 0);
  return 0;
}

void foo (__m128 *dst, __m64 *src, int n)
{
  __m128 xmm0 = { 0 };
  while (n > 64)
    {
      puts ("");
      xmm0 = __builtin_ia32_cvtpi2ps (xmm0, *src);
      *dst = xmm0;
      n --;
    }
}
