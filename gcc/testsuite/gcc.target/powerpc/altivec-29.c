/* PR target/39558 */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec -save-temps" } */
/* { dg-require-effective-target powerpc_altivec } */

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

