/* PR tree-optimization/79737 */

#pragma pack(1)
struct S
{
  int b:18;
  int c:1;
  int d:24;
  int e:15;
  int f:14;
} i;
int g, j, k;
static struct S h;

void
foo ()
{
  for (j = 0; j < 6; j++)
    k = 0;
  for (; k < 3; k++)
    {
      struct S m = { 5, 0, -5, 9, 5 };
      h = m;
      if (g)
	i = m;
      h.e = 0;
    }
}

int
main ()
{
  foo ();
  if (h.e != 0)
    __builtin_abort ();
  return 0;
}
