/* PR tree-optimization/51581 */

#include "tree-vect.h"

int a[8], b[8];
unsigned int c[8], d[8];

void
f1 (void)
{
  a[0] = b[0] / 8;
  a[1] = b[1] / 8;
  a[2] = b[2] / 8;
  a[3] = b[3] / 8;
  a[4] = b[4] / 8;
  a[5] = b[5] / 8;
  a[6] = b[6] / 8;
  a[7] = b[7] / 8;
}

void
f2 (void)
{
  c[0] = d[0] / 3;
  c[1] = d[1] / 3;
  c[2] = d[2] / 3;
  c[3] = d[3] / 3;
  c[4] = d[4] / 3;
  c[5] = d[5] / 3;
  c[6] = d[6] / 3;
  c[7] = d[7] / 3;
}

void
f3 (void)
{
  a[0] = b[0] / 8;
  a[1] = b[1] / 4;
  a[2] = b[2] / 8;
  a[3] = b[3] / 4;
  a[4] = b[4] / 8;
  a[5] = b[5] / 4;
  a[6] = b[6] / 8;
  a[7] = b[7] / 4;
}

void
f4 (void)
{
  c[0] = d[0] / 3;
  c[1] = d[1] / 5;
  c[2] = d[2] / 3;
  c[3] = d[3] / 5;
  c[4] = d[4] / 3;
  c[5] = d[5] / 5;
  c[6] = d[6] / 3;
  c[7] = d[7] / 5;
}

void
f5 (void)
{
  a[0] = b[0] / 14;
  a[1] = b[1] / 15;
  a[2] = b[2] / 14;
  a[3] = b[3] / 15;
  a[4] = b[4] / 14;
  a[5] = b[5] / 15;
  a[6] = b[6] / 14;
  a[7] = b[7] / 15;
}

void
f6 (void)
{
  c[0] = d[0] / 6;
  c[1] = d[1] / 5;
  c[2] = d[2] / 6;
  c[3] = d[3] / 5;
  c[4] = d[4] / 6;
  c[5] = d[5] / 5;
  c[6] = d[6] / 13;
  c[7] = d[7] / 5;
}

int
main ()
{
  int i;
  check_vect ();
  asm ("");
  for (i = 0; i < 8; i++)
    {
      asm ("");
      b[i] = i - 4;
      d[i] = i - 4;
    }
  f1 ();
  f2 ();
#pragma GCC novector
  for (i = 0; i < 8; i++)
    if (a[i] != b[i] / 8 || c[i] != d[i] / 3)
      abort ();
  f3 ();
  f4 ();
#pragma GCC novector
  for (i = 0; i < 8; i+= 2)
    if (a[i] != b[i] / 8 || a[i + 1] != b[i + 1] / 4
	|| c[i] != d[i] / 3 || c[i + 1] != d[i + 1] / 5)
      abort ();
  f5 ();
  f6 ();
#pragma GCC novector
  for (i = 0; i < 8; i+= 2)
    if (a[i] != b[i] / 14 || a[i + 1] != b[i + 1] / 15
	|| c[i] != d[i] / (i == 6 ? 13 : 6) || c[i + 1] != d[i + 1] / 5)
      abort ();
  return 0;
}

