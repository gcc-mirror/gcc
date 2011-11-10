/* PR rtl-optimization/51023 */

extern void abort (void);

short int
foo (long int x)
{
  return x;
}

int
main ()
{
  long int a = 0x4272AL;
  if (foo (a) == a)
    abort ();
  return 0;
}
