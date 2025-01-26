#include "tree-vect.h"

static unsigned long g_270[5][2] = {{123}};
static short g_2312 = 0;
int main()
{
  check_vect ();
  int g_1168 = 0;
  unsigned t = 4;
  for (g_1168 = 3; g_1168 >= 0; g_1168 -= 1)
    for (g_2312 = 0; g_2312 <= 1; g_2312 += 1)
      t = g_270[g_1168][0];
  if (t != 123) __builtin_abort();
}

