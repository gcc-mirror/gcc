/* { dg-do compile } */
/* { dg-additional-options "-O1 -ftree-slp-vectorize -march=znver1 -fdump-tree-optimized" } */

/* Use -O2, but -ftree-slp-vectorize option should be preserved and used.  */
#pragma GCC optimize "-O2"

typedef struct {
  long n[4];
} secp256k1_fe;

void *a;
int c;
static void
fn1(secp256k1_fe *p1, int p2)
{
  p1->n[0] = p1->n[1] = p2;
}
void
fn2()
{
  fn1(a, !c);
}

/* { dg-final { scan-tree-dump { MEM <vector\(2\) long int> \[[^]]*\] = } "optimized" } } */
