/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-fre4" } */

extern void foo(void);

static int a[2], b, *c[2];

int main() {
  for (b = 0; b < 2; b++)
    c[b] = &a[1];
  if (!c[0])
    foo();
  return 0;
}

/* Even when vectorizing we should eliminate the call to foo.  */
/* { dg-final { scan-tree-dump-not "foo" "fre4" } } */
