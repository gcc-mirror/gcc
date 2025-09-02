/* PR118839: Check that error is diagnosed when the variant is the same as
   the base function.  */

/* No previous declaration.  */
#pragma omp declare variant(f) match(user={condition(1)})  /* { dg-error "variant 'f' is the same as base function" } */
void f(int *x);

/* Previous declaration.  */
void g(int *x)
{
  *x = 42;
}

#pragma omp declare variant(g) match(user={condition(1)})  /* { dg-error "variant 'g' is the same as base function" } */
void g(int *x);



