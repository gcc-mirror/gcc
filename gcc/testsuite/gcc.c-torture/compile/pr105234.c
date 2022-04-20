/* PR target/105234 */
/* { dg-do compile } */

static inline __attribute__((always_inline)) int foo (int x) { return x + 1; }
#pragma GCC push_options
static inline __attribute__((always_inline)) int bar (int x) { return x + 2; }
#pragma GCC pop_options
static inline __attribute__((always_inline)) int baz (int x) { return x + 3; }

int
qux (void)
{
  return foo (bar (baz (42)));
}
