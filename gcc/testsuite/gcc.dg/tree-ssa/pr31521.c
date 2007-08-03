/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

void fail(void) __attribute__((noreturn));
int bar(int);

int foo(int x) {
  int i;
  int s = 0;

  if (x <= 0) fail();
  for (i = 0; i < x; ++i) {
    /* This division by 4 should be replaced with >> 2.  */
    s += bar(i/4);
  }
  return s;
}

/* { dg-final { scan-tree-dump-times " = i_.* >> 2" 1 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
