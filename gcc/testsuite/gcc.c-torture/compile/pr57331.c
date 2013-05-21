/* PR tree-optimization/57331 */

int
foo (int x)
{
  void *p = x ? (void *) 1 : (void *) 0;
  __INTPTR_TYPE__ b = (__INTPTR_TYPE__) p;
  if (b)
    return 0;
  return 1;
}
