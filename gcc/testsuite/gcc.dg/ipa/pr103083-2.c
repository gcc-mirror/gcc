/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-bit-cp -fdump-tree-optimized" } */

struct b {int b;};
struct a {int a; struct b b;};

void remove_any_mention (void);

__attribute__ ((noinline))
static void test2 (struct b *b)
{
  if (b)
    remove_any_mention ();
}

__attribute__ ((noinline))
static void
test (struct a *a)
{
  test2(a? &a->b : 0);
}

int
foo()
{
  test(0);
  return 0;
}

/* { dg-final { scan-tree-dump-not "remove_any_mention" "optimized" } } */
