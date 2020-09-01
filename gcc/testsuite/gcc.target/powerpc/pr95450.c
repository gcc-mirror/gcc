/* PR target/95450 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "return \[0-9.e+]\+;" "optimized" } } */

/* Verify this is not optimized for double double into return floating_point_constant,
   as while that constant is the maximum normalized floating point value, it needs
   107 bit precision, which is more than GCC supports for this format.  */

#if __LDBL_MANT_DIG__ == 106
union U
{
  struct { double hi; double lo; } dd;
  long double ld;
};

const union U g = { { __DBL_MAX__, __DBL_MAX__ / (double)134217728UL / (double)134217728UL } };
#else
struct S
{
  long double ld;
} g;
#endif

long double
foo (void)
{
  return g.ld;
}
