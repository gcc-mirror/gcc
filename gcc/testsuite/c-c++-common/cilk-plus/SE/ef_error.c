/* { dg-do compile } */
/* { dg-options "-fcilkplus -fopenmp" } */
/* { dg-require-effective-target fopenmp } */

#pragma omp declare simd linear(y:1) simdlen(4) 
__attribute__((vector (linear (y:1), vectorlength(4))))
int func (int x, int y) { /* { dg-error "cannot be used in the same function marked as a Cilk Plus SIMD-enabled" } */ 
  return (x+y);
}
__attribute__((vector (linear (y:1), private (x)))) /* { dg-error "is not valid for" } */
int func2 (int x, int y) {
  return (x+y);
}

__attribute__((vector (linear (y:1), simdlen (4)))) /* { dg-error "is not valid for" } */
int func2_1 (int x, int y) {
  return (x+y);
}

__attribute__((vector (linear (y:1), inbranch))) /* { dg-error "is not valid for" } */
int func2_3 (int x, int y) {
  return (x+y);
}

__attribute__((vector (notinbranch, vectorlength (4)))) /* { dg-error "is not valid for" } */
int func2_2 (int x, int y) {
  return (x+y);
}

int main (void)
{
  return (func (5,6));
}
