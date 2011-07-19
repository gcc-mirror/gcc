/* PR middle-end/48973 */

extern void abort (void);
struct S { int f : 1; } s;
int v = -1;

void
foo (unsigned int x)
{
  if (x != -1U)
    abort ();
}

int
main ()
{
  s.f = (v & 1) > 0;
  foo (s.f);
  return 0;
}
