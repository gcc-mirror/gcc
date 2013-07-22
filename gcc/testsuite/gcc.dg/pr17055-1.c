/* PR middle-end/17055.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

/* This test used to abort, because we do an "integer" fold to zero, i.e.
   x - x = (T)0 where T is the type of x.  Unfortunately, fold_convert
   was unable to convert integer_zero_node to the appropriate vector type.  */

typedef float v4sf __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));

v4sf ivf, ovf;
v4si ivi, ovi;

void testf (void)
{
  ovf = ivf - ivf;
}

void testi (void)
{
  ovi = ivi - ivi;
}

