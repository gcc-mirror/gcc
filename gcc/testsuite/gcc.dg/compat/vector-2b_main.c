/* { dg-skip-if "test AVX support" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target avx_runtime } */

/* Test compatibility of vector types: layout between separately-compiled
   modules, parameter passing, and function return.  This test uses
   vectors of floating points values.  */

extern void vector_2_x (void);
extern void exit (int);
int fails;

int
main ()
{
  vector_2_x ();
  exit (0);
}
