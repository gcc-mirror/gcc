/* Verify that ifcvt doesn't crash under a number of interesting conditions. */

void bar (void);
void baz (void);

void f1(int p)
{
  if (p)
    __builtin_trap();
}

void f2(int p)
{
  if (p)
    __builtin_trap();
  else
    bar();
}

void f3(int p)
{
  if (p)
    bar();
  else
    __builtin_trap();
}

void f4(int p, int q)
{
  if (p)
    {
      bar();
      if (q)
	bar();
    }
  else
    __builtin_trap();
}

void f5(int p)
{
  if (p)
    __builtin_trap();
  else
    __builtin_abort();
}

void f6(int p)
{
  if (p)
    __builtin_abort();
  else
    __builtin_trap();
}

void f7(int p)
{
  if (p)
    __builtin_trap();
  else
    __builtin_trap();
}

void f8(int p)
{
  if (p)
    __builtin_trap();
  else
    {
      bar();
      __builtin_trap();
    }
}

void f9(int p)
{
  if (p)
    {
      bar();
      __builtin_trap();
    }
  else
    __builtin_trap();
}

void f10(int p)
{
  if (p)
    __builtin_trap();
  while (1)
    bar();
}

void f11(int p)
{
  if (p)
    __builtin_trap();
  else
    bar();
  while (1)
    baz();
}
