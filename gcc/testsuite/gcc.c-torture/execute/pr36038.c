/* PR tree-optimization/36038 */

long long list[10];
long long expect[10] = { 0, 1, 2, 3, 4, 4, 5, 6, 7, 9 };
long long *stack_base;
int indices[10];
int *markstack_ptr;

void
doit (void)
{
  long long *src;
  long long *dst;
  long long *sp = stack_base + 5;
  int diff = 2;
  int shift;
  int count;

  shift = diff - (markstack_ptr[-1] - markstack_ptr[-2]);
  count = (sp - stack_base) - markstack_ptr[-1] + 2;
  src = sp;
  dst = (sp += shift);
  while (--count)
    *dst-- = *src--;
}

int
main ()
{
  int i;
  for (i = 0; i < 10; i++)
    list[i] = i;

  markstack_ptr = indices + 9;
  markstack_ptr[-1] = 2;
  markstack_ptr[-2] = 1;

  stack_base = list + 2;
  doit ();
  if (__builtin_memcmp (expect, list, sizeof (list)))
    __builtin_abort ();
  return 0;
}
