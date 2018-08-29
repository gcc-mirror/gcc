/* PR middle-end/83977 */
/* { dg-do compile } */

void bar (void);
int foo (int, int) __attribute__((used));

#pragma omp declare simd uniform (b) linear(a:b)
int
foo (int a, int b)
{
  a = a + 1;
/* This function can't be called from simd loops,
   because it violates declare simd restrictions.
   We shouldn't ICE on it though, nor attempt to generate
   simd clones for the *omp_fn* functions.  */
  #pragma omp parallel
  bar ();  
  return a;
}

int foo (int, int)  __attribute__((unused));
