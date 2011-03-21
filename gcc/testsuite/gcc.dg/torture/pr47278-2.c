/* { dg-require-visibility "" } */

extern void abort (void);

int __attribute__((weak,visibility("hidden"))) foo (void)
{
  return 0;
}

int main()
{
  if (foo() != 1)
    abort ();
  return 0;
}
