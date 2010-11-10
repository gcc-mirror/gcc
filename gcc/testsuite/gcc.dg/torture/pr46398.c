/* { dg-do run } */

int __attribute__((noinline))
test (int token)
{
  int done = 0;
  int virtual_p = 0;
  while (!done)
    {
      if (token == 42)
	virtual_p = 1;
      else
	done = 1;
    }
  return virtual_p;
}
extern void abort (void);
int
main()
{
  if (test (0) != 0)
    abort ();
  return 0;
}
