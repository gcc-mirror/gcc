#include <limits.h>

#if INT_MAX <= 32767
int main () { exit (0); }
#else
void get_addrs (const char**x, int *y)
{
  x[0] = "a1111" + (y[0] - 0x10000) * 2;
  x[1] = "a1112" + (y[1] - 0x20000) * 2;
  x[2] = "a1113" + (y[2] - 0x30000) * 2;
  x[3] = "a1114" + (y[3] - 0x40000) * 2;
  x[4] = "a1115" + (y[4] - 0x50000) * 2;
  x[5] = "a1116" + (y[5] - 0x60000) * 2;
  x[6] = "a1117" + (y[6] - 0x70000) * 2;
  x[7] = "a1118" + (y[7] - 0x80000) * 2;
}

int main ()
{
  const char *x[8];
  int y[8];
  int i;

  for (i = 0; i < 8; i++)
    y[i] = 0x10000 * (i + 1);
  get_addrs (x, y);
  for (i = 0; i < 8; i++)
    if (*x[i] != 'a')
      abort ();
  exit (0);
}
#endif
