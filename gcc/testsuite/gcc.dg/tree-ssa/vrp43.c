/* { dg-do run } */
/* { dg-options "-O2" } */

int __attribute__((noinline))
foo(int i)
{
  if (i != 0)
    {
      char c = (char)i;
      return c != 0;
    }
  return 0;
}

extern void abort (void);

int main()
{
  if (foo(0xff00))
    abort ();
  return 0;
}
