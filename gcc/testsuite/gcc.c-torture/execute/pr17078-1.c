extern void abort(void);

void test(int *ptr)
{
  int i = 1;
  goto useless;
  if (0)
    {
      useless:
      i = 0;
    }
  else
    i = 1;
  *ptr = i;
}

int main()
{
  int i = 1;
  test(&i);
  if (i)
    abort ();
  return 0;
}

