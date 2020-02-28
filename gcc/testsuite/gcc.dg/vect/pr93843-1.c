char a;
struct S { short b, c; } d;

__attribute__((noipa)) void
foo (int x)
{
  if (x != 4)
    __builtin_abort ();
}

int
main ()
{
  short *g = &d.c, *h = &d.b;
  char e = 4 - a;
  int f;
  *h = *g = e;
  for (f = 0; f < 2; f++)
    foo (d.c);
  return 0;
}
