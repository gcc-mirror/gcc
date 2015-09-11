/* PR target/39558 */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -save-temps" } */

#define ATTRIBUTE_UNUSED __attribute__((unused))

int *foo (int *vector)
{
  return vector;
}

int *bar (int *vector ATTRIBUTE_UNUSED)
{
  return vector;
}

int *baz (int *vector __attribute__((unused)))
{
  return vector;
}

