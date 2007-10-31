int foo(int i)
{
  return ((int)((unsigned)(i + 1) * 4)) / 4;
}

extern void abort(void);
int main()
{
  if (foo(0x3fffffff) != 0)
    abort ();
  return 0;
}
