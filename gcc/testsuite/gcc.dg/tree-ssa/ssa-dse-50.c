/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

extern void foo(void);
static int a, *c, g, **j;
int b;
static void e() {
  int k, *l[5] = {&k, &k, &k, &k, &k};
  while (g) {
    j = &l[0];
    b++;
  }
}
static void d(int m) {
  int **h[30] = {&c}, ***i[1] = {&h[3]};
  if (m)
    foo();
  e();
}
int main() {
  d(a);
  return 0;
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 8 "dse1" } } */
