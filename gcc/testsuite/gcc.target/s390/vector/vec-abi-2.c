/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* This needs to be v24 = v24 * v26 + v28 */
/* { dg-final { scan-assembler "vfmadb\t%v24,%v24,%v26,%v28" } } */

typedef double v2df __attribute__((vector_size(16)));

v2df
madd (v2df a, v2df b, v2df c)
{
  return a * b + c;
}
