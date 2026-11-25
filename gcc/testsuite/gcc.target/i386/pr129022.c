/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O -Wno-psabi" } */

typedef long double V __attribute__((vector_size(32)));
typedef unsigned short U __attribute__((vector_size(32)));

U foo (void)
{
  return (U)(V){ 1.0L, 1.0L };
}
