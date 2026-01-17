/* { dg-do run } */

/* PR tree-optimization/123645 */
/* Phi-opt was transforming f into __builtin_bswap which was incorrect. */

__attribute__((noinline))
int f(unsigned a)
{
  return a < 1 ? __builtin_bswap32(a) : 0;
}

int main()
{
  if (f(1) != 0)
    __builtin_abort();
  if (f(0) != 0)
    __builtin_abort();
  if (f(2) != 0)
    __builtin_abort();
  if (f(3) != 0)
    __builtin_abort();
  if (f(-1) != 0)
    __builtin_abort();
}
