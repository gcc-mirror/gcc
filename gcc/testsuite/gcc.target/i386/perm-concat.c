/* { dg-do compile } */
/* { dg-options "-O -mavx -mfpmath=sse" } */

typedef double v2df __attribute__ ((__vector_size__ (16)));

v2df
f (double d)
{
  v2df x = {-d, d};
  return __builtin_ia32_vpermilpd (x, 1);
}

/* { dg-final { scan-assembler-not "\tvpermilpd\[ \t\]" } } */
