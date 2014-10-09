/* { dg-require-effective-target int32plus } */
extern void abort (void);

struct s
{
  unsigned long long a:16;
  unsigned long long b:32;
  unsigned long long c:16;
};

__attribute__ ((noinline)) unsigned
f (struct s s, unsigned i)
{
  return s.b == i;
}

struct s s = { 1, 0x87654321u, 2};

int
main ()
{
  if (!f (s, 0x87654321u))
    abort ();
  return 0;
}
