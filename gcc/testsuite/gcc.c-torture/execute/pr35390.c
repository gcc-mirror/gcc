extern void abort (void);

unsigned int foo (int n)
{
  return ~((unsigned int)~n);
}

int main()
{
  if (foo(0) != 0)
    abort ();
  return 0;
}
