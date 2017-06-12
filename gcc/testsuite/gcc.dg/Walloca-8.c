/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=2000 -O2" } */

void *p;
void
foo (__SIZE_TYPE__ len)
{
  if (len < 2000 / sizeof (void *))
    p = __builtin_alloca (len * sizeof (void *));
  else
    p = __builtin_malloc (len * sizeof (void *));
}
