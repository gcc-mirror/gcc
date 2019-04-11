/* { dg-do run } */
/* { dg-require-weak "" } */

void __attribute__((noinline,noclone))
check (int i)
{
  if (i == 0)
    __builtin_exit (0);
}

int i;
extern int x __attribute__((weak));

int main(int argc, char **argv)
{
  if (argc)
    {
      check (i);
      return x;
    }
  else
    {
      check (i);
      return x-1;
    }
  return 0;
}
