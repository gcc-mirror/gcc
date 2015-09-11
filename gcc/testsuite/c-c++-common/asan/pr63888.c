/* PR bootstrap/63888 */
/* { dg-do run } */

__attribute__((noinline, noclone)) int
foo (int x)
{
  int v = 0;
  switch (x)
    {
    case 11: v = 67; break;
    case 12: v = 68; break;
    case 13: v = 69; break;
    }
  return v;
}

__attribute__((noinline, noclone)) int
bar (int x)
{
  int v = 0;
  switch (x)
    {
    case 18: v = 67; break;
    case 19: v = 68; break;
    case 20: v = 69; break;
    }
  return v;
}

int
main ()
{
  return foo (11) - 67 + bar (19) - 68;
}
