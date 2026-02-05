/* PR middle-end/123978 */

struct A { unsigned b, c, *d; };

[[gnu::noipa]] int
foo (struct A *a)
{
  __builtin_memset (a->d, 0, ((long long) sizeof (unsigned)) * a->b * a->c);
  return 0;
}

int
main ()
{
  struct A a;
  unsigned b[256];
  __builtin_memset (b, 0x55, sizeof (b));
  a.b = 15;
  a.c = 15;
  a.d = b;
  foo (&a);
  for (int i = 0; i < 225; ++i)
    if (b[i] != 0)
      __builtin_abort ();
}
