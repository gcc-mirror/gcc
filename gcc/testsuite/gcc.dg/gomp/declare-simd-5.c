/* Test parsing of #pragma omp declare simd */
/* { dg-do compile } */

int
f1 (int x)
{
  if (x)
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
    extern int f3 (int a, int *b, int c);	/* { dg-error "must be followed by function declaration or definition" } */
  while (x < 10)
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
    extern int f4 (int a, int *b, int c);	/* { dg-error "must be followed by function declaration or definition" } */
  {
lab:
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
    extern int f5 (int a, int *b, int c);	/* { dg-error "must be followed by function declaration or definition" } */
    x++;					/* { dg-error "expected expression before" "" { target *-*-* } .-1 } */
  }
  return x;
}

int
f2 (int x)
{
  if (x)
    extern int f6 (int a, int *b, int c);	/* { dg-error "expected expression before" } */
  while (x < 10)
    extern int f7 (int a, int *b, int c);	/* { dg-error "expected expression before" } */
  {
lab:
    extern int f8 (int a, int *b, int c);	/* { dg-error "a label can only be part of a statement and a declaration is not a statement" } */
    x++;
  }
  return x;
}
