/* PR middle-end/48973 */

extern void abort (void);
struct S { int f : 1; } s;
int v = -1;

int
main ()
{
  s.f = v < 0;
  if ((unsigned int) s.f != -1U)
    abort ();
  return 0;
}
