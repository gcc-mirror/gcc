/* { dg-do compile } */
/* { dg-options "-Walloca-larger-than=256 -O2" } */
/* { dg-xfail-if "Currently broken but Andrew's work should fix this" { *-*-* } } */

void f (void*);
void g (__SIZE_TYPE__ n)
{
  // No warning on this case.  Range is easily determinable.
  if (n > 0 && n < 256)
    f (__builtin_alloca (n));
}
