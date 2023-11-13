/* { dg-add-options stack_size } */

#ifdef STACK_SIZE
#define SIZE STACK_SIZE / 8
#else
#define SIZE 65536
#endif

void
memtst (int *p, int a)
{
  do
    {
      if (p[a] == 1)
	break;
    }
  while (--a);
}

int
main (void)
{
  int a[SIZE];
  int i;
  __builtin_bzero (a, SIZE * 4);
  for (i = 0;  i < 100;  i++)
    {
      memtst (a, SIZE);
    }
}
