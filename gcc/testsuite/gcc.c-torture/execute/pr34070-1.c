extern void abort (void);

int f(unsigned int x)
{
    return ((int)x) % 4;
}

int main()
{
  if (f(-1) != -1)
    abort ();
  return 0;
}
