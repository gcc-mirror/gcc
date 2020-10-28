/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=128 -O2" } */

void f (void*);

void g (unsigned int n)
{
  if (n == 7)
    n = 11;
  f (__builtin_alloca (n)); /* { dg-warning "may be too large" } */
}
