extern void abort(void);
void x(void *data)
{
  if ((long)data < 0)
    abort();
}
int main()
{
  long i;
  for (i = 0; i < 5; i++)
    if (i > 0)
      x((void *)(i - 1));
  return 0;
}

