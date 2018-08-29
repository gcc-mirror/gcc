/* PR middle-end/30262 */
/* { dg-skip-if "asm statements do not work as expected" { rl78-*-* } } */
extern void abort (void);

int
foo (void)
{
  unsigned int x = 0;

  void nested (void)
  {
    x = 254;
  }

  nested ();
  asm volatile ("" :: "r" (x));
  asm volatile ("" :: "m" (x));
  asm volatile ("" :: "mr" (x));
  asm volatile ("" : "=r" (x) : "0" (x));
  asm volatile ("" : "=m" (x) : "m" (x));
  return x;
}

int
bar (void)
{
  unsigned int x = 0;

  void nested (void)
  {
    asm volatile ("" :: "r" (x));
    asm volatile ("" :: "m" (x));
    asm volatile ("" :: "mr" (x));
    x += 4;
    asm volatile ("" : "=r" (x) : "0" (x));
    asm volatile ("" : "=m" (x) : "m" (x));
  }

  nested ();
  return x;
}

int
baz (void)
{
  unsigned int x = 0;

  void nested (void)
  {
    void nested2 (void)
    {
      asm volatile ("" :: "r" (x));
      asm volatile ("" :: "m" (x));
      asm volatile ("" :: "mr" (x));
      x += 4;
      asm volatile ("" : "=r" (x) : "0" (x));
      asm volatile ("" : "=m" (x) : "m" (x));
    }
    nested2 ();
    nested2 ();
  }

  nested ();
  return x;
}

int
main (void)
{
  if (foo () != 254 || bar () != 4 || baz () != 8)
    abort ();
  return 0;
}
