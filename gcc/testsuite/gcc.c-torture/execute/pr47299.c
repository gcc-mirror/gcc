/* PR rtl-optimization/47299 */

extern void abort (void);

__attribute__ ((noinline, noclone)) unsigned short
foo (unsigned char x)
{
  return x * 255;
}

int
main ()
{
  if (foo (0x40) != 0x3fc0)
    abort ();
  return 0;
}
