void foo(void *);
void
MMAPGCD (int *A1, int *A2)
{
  int *t;

  do
    {
      t = A1;
      A1 = A2;
      A2 = t;
    }
  while (A2[-1]);

  foo (A1-1);
  foo (A2-1);
}

