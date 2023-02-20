/* { dg-options "-O2 -fdump-tree-optimized" } */

int b;
int *c;
int e;
static int *f = &e;
int g;
void foo();
short(a)(short h, short i) { return h - i; }
int(d)(int h) { return h == 83647 ? 0 : -h; }
int main() {
  short j;
  int *k = &e, *l = &b;
  *f = 0 == c;
  j = a(0 != 2, *k);
  if (d(j ^ (0 == l || *k)) != *k)
    ;
  else
    foo();
  c = &g;
}

/* { dg-final { scan-tree-dump-times " 1 - " 0 "optimized" } } */
/* There should be no calls to foo. */
/* { dg-final { scan-tree-dump-times "foo " 0 "optimized" } } */

