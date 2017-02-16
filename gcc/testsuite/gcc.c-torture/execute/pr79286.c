int a = 0, c = 0;
static int d[][8] = {};

int main ()
{
  int e;
  for (int b = 0; b < 4; b++)
    {
      __builtin_printf ("%d\n", b, e);
      while (a && c++)
	e = d[300000000000000000][0];
    }

  return 0;
}
