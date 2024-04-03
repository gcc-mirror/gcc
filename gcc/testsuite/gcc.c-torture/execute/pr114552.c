/* PR middle-end/114552 */

struct __attribute__((packed)) S { short b; int c; };
struct T { struct S b; int e; };
static const struct T k = { { 1, 0 }, 0 };

__attribute__((noinline)) void
foo (void)
{
  asm volatile ("" : : : "memory");
}

__attribute__((noinline)) void
bar (struct S n)
{
  foo ();
}

int
main ()
{
  bar (k.b);
  return 0;
}
