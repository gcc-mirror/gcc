/* PR target/39240 */

extern void abort (void);

__attribute__ ((noinline))
static int foo1 (int x)
{
  return x;
}

__attribute__ ((noinline))
unsigned int bar1 (int x)
{
  return foo1 (x + 6);
}

volatile unsigned long l1 = (unsigned int) -4;

__attribute__ ((noinline))
static short int foo2 (int x)
{
  return x;
}

__attribute__ ((noinline))
unsigned short int bar2 (int x)
{
  return foo2 (x + 6);
}

volatile unsigned long l2 = (unsigned short int) -4;

__attribute__ ((noinline))
static signed char foo3 (int x)
{
  return x;
}

__attribute__ ((noinline))
unsigned char bar3 (int x)
{
  return foo3 (x + 6);
}

volatile unsigned long l3 = (unsigned char) -4;

__attribute__ ((noinline))
static unsigned int foo4 (int x)
{
  return x;
}

__attribute__ ((noinline))
int bar4 (int x)
{
  return foo4 (x + 6);
}

volatile unsigned long l4 = (int) -4;

__attribute__ ((noinline))
static unsigned short int foo5 (int x)
{
  return x;
}

__attribute__ ((noinline))
short int bar5 (int x)
{
  return foo5 (x + 6);
}

volatile unsigned long l5 = (short int) -4;

__attribute__ ((noinline))
static unsigned char foo6 (int x)
{
  return x;
}

__attribute__ ((noinline))
signed char bar6 (int x)
{
  return foo6 (x + 6);
}

volatile unsigned long l6 = (signed char) -4;

int
main (void)
{
  if (bar1 (-10) != l1)
    abort ();
  if (bar2 (-10) != l2)
    abort ();
  if (bar3 (-10) != l3)
    abort ();
  if (bar4 (-10) != l4)
    abort ();
  if (bar5 (-10) != l5)
    abort ();
  if (bar6 (-10) != l6)
    abort ();
  return 0;
}
