/* { dg-require-effective-target untyped_assembly } */
struct S1
{
  short f0;
};
extern volatile struct S1 g_5;
extern int g_120, i;
extern short g_339;

int
func_72 (int x, int y, struct S1 z)
{
  for (z.f0 = -3; z.f0 > 16; z.f0 += 1)
    foo ();
  return g_120;
}

int
main ()
{
  while (g_339 % 0x200003)
    continue;
  if (i)
    func_72 (0, 0, g_5);
  foo (&g_339);
  return 0;
}
