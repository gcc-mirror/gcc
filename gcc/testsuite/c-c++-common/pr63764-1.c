/* PR target/63764 */
/* { dg-do compile } */

#define A __attribute__((vector_size (4 * sizeof (float))))
typedef float V A;

void
fn1 (V *x)
{
  V a = *x;
  ((V) a)[0] = 0;	/* { dg-error "lvalue required as left operand of assignment" } */
  *x = a;
}

void
fn2 (V *x)
{
  float A a = *x;
  ((float A) a)[0] = 0;	/* { dg-error "lvalue required as left operand of assignment" } */
  *x = a;
}
