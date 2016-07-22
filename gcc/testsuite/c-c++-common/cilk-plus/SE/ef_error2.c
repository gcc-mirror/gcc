/* The warning is x86-only.  */
/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus -Wall" } */

__attribute__((vector (vectorlength(128)))) 
//#pragma omp simd simdlen (128)
int func2 (int x, int y)  /* { dg-warning "unsupported simdlen" } */
{
  return (x+y);
}

int main (void)
{
  return (func2 (5,6));
}
