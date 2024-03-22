/* PR tree-optimization/113013 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O2" } */

struct S { short x; } s;
void *foo () __attribute__((__alloc_size__(1)));
struct S *p;

__SIZE_TYPE__
bar (void)
{
  p = foo (s);
  return __builtin_dynamic_object_size (p, 0);
}
