/* PR target/104327 */

void foo (int *);

static inline __attribute__((always_inline)) void
bar (int *x)
{
  foo (x);
}

__attribute__((cold, optimize("Os"))) void
baz (int *x)
{
  bar (x);
}
