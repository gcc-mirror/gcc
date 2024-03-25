void bar (void);

void foo(int p, int q)
{
  if (p)
    {
      if (q)
        __builtin_trap ();
    }
  else
    bar();
}
