/* { dg-do compile } */
/* { dg-additional-options "-ftree-slp-vectorize" } */

typedef struct {
  long n[5];
} secp256k1_fe;

void *a, *b;
int c;
static void
fn1(secp256k1_fe *p1, int p2)
{
  p1->n[2] = p1->n[3] = p1->n[4] = p2;
}
void
fn2()
{
  fn1(b, !c);
  fn1(a, !c);
}
