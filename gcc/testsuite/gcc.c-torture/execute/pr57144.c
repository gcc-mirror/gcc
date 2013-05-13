extern void abort (void);
extern void exit (int);

void __attribute__ ((noinline))
foo(int a)
{
  int z = a > 0 ? a : -a;
  long long x = z;
  if (x > 0x100000000LL)
    abort ();
  else
    exit (0);
}

int
main()
{
  foo (1);
}
