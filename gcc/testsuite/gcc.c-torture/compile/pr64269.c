/* PR tree-optimization/64269 */

void
foo (char *p)
{
  __SIZE_TYPE__ s = ~(__SIZE_TYPE__)0;
  *p = 0;
  __builtin_memset (p + 1, 0, s);
}
