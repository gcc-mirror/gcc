/* { dg-do run } */

__attribute__ ((noinline, noclone)) int
foo (char *c)
{
  asm volatile ("" : : "r" (c) : "memory");
  return 1;
}

__attribute__ ((noinline, noclone)) void
bar (char *c)
{
  asm volatile ("" : : "r" (c) : "memory");
}

int main ()
{
  char tpl[20] = "/tmp/test.XXXXXX";
  char tpl2[20] = "/tmp/test.XXXXXX";
  int fd = foo (tpl);
  int fd2 = foo (tpl2);
  if (fd == -1)
    {
      if (fd2 != -1)
	 bar (tpl2);
      return 1;
    }

  if (fd2 == -1)
    return 1;

  bar (tpl);
  bar (tpl2);

  if (__builtin_strcmp (tpl, "/tmp/test.XXXXXX") != 0)
    return 1;

  if (__builtin_strcmp (tpl, tpl2) != 0)
    return 1;

   return 0;
}
