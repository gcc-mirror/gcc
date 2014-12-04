/* PR target/63764 */
/* { dg-do compile } */

#define A __attribute__((vector_size (4 * sizeof (float))))
typedef float V A;

float
fn1 (V *x)
{
  V a = *x;
  return ((V) a)[0];
}

float
fn2 (V *x)
{
  float A a = *x;
  return ((float A) a)[0];
}

void
fn3 (V *x)
{
  V a = *x;
  a[0] = 0;
  *x = a;
}

void
fn4 (V *x)
{
  float A a = *x;
  a[0] = 0;
  *x = a;
}
