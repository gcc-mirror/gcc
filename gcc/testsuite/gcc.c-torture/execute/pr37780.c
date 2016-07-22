/* PR middle-end/37780.  */

#define VAL (8 * sizeof (int))

int __attribute__ ((noinline, noclone))
fooctz (int i)
{
  return (i == 0) ? VAL : __builtin_ctz (i);
}

int __attribute__ ((noinline, noclone))
fooctz2 (int i)
{
  return (i != 0) ? __builtin_ctz (i) : VAL;
}

unsigned int __attribute__ ((noinline, noclone))
fooctz3 (unsigned int i)
{
  return (i > 0) ?  __builtin_ctz (i) : VAL;
}

int __attribute__ ((noinline, noclone))
fooclz (int i)
{
  return (i == 0) ? VAL : __builtin_clz (i);
}

int __attribute__ ((noinline, noclone))
fooclz2 (int i)
{
  return (i != 0) ? __builtin_clz (i) : VAL;
}

unsigned int __attribute__ ((noinline, noclone))
fooclz3 (unsigned int i)
{
  return (i > 0) ? __builtin_clz (i) : VAL;
}

int
main (void)
{
  if (fooctz (0) != VAL || fooctz2 (0) != VAL || fooctz3 (0) != VAL
      || fooclz (0) != VAL || fooclz2 (0) != VAL || fooclz3 (0) != VAL)
    __builtin_abort ();

  return 0;
}