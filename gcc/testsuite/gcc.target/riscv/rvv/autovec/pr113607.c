/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -fdump-tree-optimized" } */

struct {
  signed b;
} c, d = {6};

short e, f;
int g[1000];
signed char h;
int i, j;
long k, l;

long m(long n, long o) {
  if (n < 1 && o == 0)
    return 0;
  return n;
}

static int p() {
  long q = 0;
  int a = 0;
  for (; e < 2; e += 1)
    g[e * 7 + 1] = -1;
  for (; h < 1; h += 1) {
    k = g[8] || f;
    l = m(g[f * 7 + 1], k);
    a = l;
    j = a < 0 || g[f * 7 + 1] < 0 || g[f * 7 + 1] >= 32 ? a : a << g[f * 7 + 1];
    if (j)
      ++q;
  }
  if (q)
    c = d;
  return i;
}

int main() {
  p();
  if (c.b != 6)
    __builtin_abort ();
}

/* We must not fold VEC_COND_EXPR into COND_SHL.
   Therefore, make sure that we still have 2/4 VCOND_MASKs with real else
   value.  */

/* { dg-final { scan-tree-dump-times { = \.VCOND_MASK.\([a-z0-9\._]+, [a-z0-9\._\{\}, ]+, [0-9\.\{\},]+\);} 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times { = \.VCOND_MASK.\([a-z0-9\._]+, [a-z0-9\._\{\}, ]+, [a-z0-9\._]+\);} 4 "optimized" } } */
