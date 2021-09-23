/* { dg-do run } */
/* { dg-additional-sources "pr79334-1.c" } */

extern int d[][8];

static void __attribute__((noinline))
func_that_exits (int flag)
{
  if (!flag)
    __builtin_exit (0);
}

int main ()
{
  int e = 0;
  while (1)
    {
      func_that_exits (e);
      /* We do not know whether d[1024][0] will trap.  */
      e = d[1024][0];
    }
  return 0;
}
