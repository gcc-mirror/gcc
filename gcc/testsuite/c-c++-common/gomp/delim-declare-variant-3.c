/* { dg-do compile } */

/* Check that an error is diagnosed when a function defined in a
   "begin declare variant" construct doesn't have a visible declaration
   at that point.  

   The spec is not completely clear on this; it says the base function must be
   "declared elsewhere without an associated declare variant directive",
   without defining what "elsewhere" means.  Particularly in C++ it would be
   incorrect to inject such a declaration at the point of the variant
   definition (think of a variant for a class method that is defined with a
   qualified name outside of the class declaration, for instance).  The C++
   front end could differentiate between cases where base declaration injection
   is allowed vs not, but for now it seems simplest just to require that a
   definition always be visible.  */

/* This declaration of baz is incompatible with the variant below.  */
extern double baz (double, double);

/* This is not a function at all.  */
extern int quux;

#pragma omp begin declare variant match (construct={target})
int foo (int a)
{
  return a + 1;
}

int bar (int x)  /* { dg-error "no declaration of base function" } */
{
  return x * 2;
}

int baz (int x)  /* { dg-error "variant function definition does not match declaration of .baz." } */
{
  return x * 2;
}

int quux (int x, int y)  /* { dg-error "variant function definition does not match declaration of .quux." } */
{
  return x + y;
}
#pragma omp end declare variant

/* It's supposedly allowed to define the base function after the variant.  */
int foo (int a)
{
  return a;
}
