int *ptr1 = 0, **ptr2 = &ptr1;

int *identity(int *p)
{
  return p;
}

void store_to_c(int *p)
{
  *ptr2 = identity(p);
}

int main()
{
  int f;
  store_to_c(&f);
  if (ptr1 != &f)
    __builtin_abort();
  return 0;
}
