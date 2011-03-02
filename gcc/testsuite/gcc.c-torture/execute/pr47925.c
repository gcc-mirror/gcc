struct s { volatile struct s *next; };

void __attribute__((noinline))
bar (int ignored, int n)
{
  asm volatile ("");
}

int __attribute__((noinline))
foo (volatile struct s *ptr, int n)
{
  int i;

  bar (0, n);
  for (i = 0; i < n; i++)
    ptr = ptr->next;
}

int main (void)
{
  volatile struct s rec = { &rec };
  foo (&rec, 10);
  return 0;
}
