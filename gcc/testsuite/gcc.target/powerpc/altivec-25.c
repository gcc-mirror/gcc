/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec -O2 -Wall" } */
/* { dg-require-effective-target powerpc_altivec } */


#define vector __attribute__((__vector_size__(16) ))
vector int f()
{
  int t = 4;
  return (vector int){t,t,t,t};
}
vector int f1()
{
  return (vector int){4,4,4,4};
}

/* We should be able to materialize the constant vector without
   any lvewx instructions as it is constant. */
/* { dg-final { scan-assembler-not "lvewx" } } */

