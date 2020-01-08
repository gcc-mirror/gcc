/* PR tree-optimization/92891 */

int a, b;
char *foo (int) __attribute__((alloc_size(1)));

void
bar (void)
{
  char *e = foo (2);
  while (a)
    {
      if (b <= 0)
	continue;
      e[b] = 0;
    }
}
