/* PR/110991 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunroll-details -fdump-tree-optimized" } */

static unsigned char a;
static signed char b;
void foo(void);
int main() {
  a = 25;
  for (; a > 13; --a)
    b = a > 127 ?: a << 3;
  if (!b)
    foo();
}

/* { dg-final { scan-tree-dump "optimized: loop with \[0-9\]\+ iterations completely unrolled" "cunroll" } } */
/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
