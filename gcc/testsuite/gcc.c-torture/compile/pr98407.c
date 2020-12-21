/* PR tree-optimization/98407 */

struct S { int a; int b[]; };
const struct S c = { 0, { 0 } }, d = { 0, { 0 } };

int
foo (void)
{
  return __builtin_memcmp (&c, &d, sizeof d);
}
