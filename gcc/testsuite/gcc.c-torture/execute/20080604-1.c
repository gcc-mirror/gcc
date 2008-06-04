struct barstruct { char const* some_string; } x;
extern void abort (void);
void __attribute__((noinline))
foo(void)
{
  if (!x.some_string)
    abort ();
}
void baz(int b)
{
  struct barstruct bar;
  struct barstruct* barptr;
  if (b)
    barptr = &bar;
  else
    {
      barptr = &x + 1;
      barptr = barptr - 1;
    }
  barptr->some_string = "Everything OK";
  foo();
  barptr->some_string = "Everything OK";
}
int main()
{
  x.some_string = (void *)0;
  baz(0);
  if (!x.some_string)
    abort ();
  return 0;
}

