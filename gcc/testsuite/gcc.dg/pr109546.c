/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

void foo(void);
static int a, c;
static int *b = &a;
static int **d = &b;
void assert_fail() __attribute__((__noreturn__));
int main() {
  int *e = *d;
  if (e == &a || e == &c);
  else {
    __builtin_unreachable();
  assert_fail();
  }
  if (e == &a || e == &c);
  else
    foo();
}

/* { dg-final { scan-tree-dump-not "assert_fail" "optimized" } } */
/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */


