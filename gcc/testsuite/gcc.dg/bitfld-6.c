/* { dg-options "" } */
#include <limits.h>

struct s
{
  int i1 : sizeof (int) * CHAR_BIT;
  int i2 : sizeof (int) * CHAR_BIT;
  int i3 : sizeof (int) * CHAR_BIT;
  int i4 : sizeof (int) * CHAR_BIT;
  int i5 : sizeof (int) * CHAR_BIT;
  int i6 : sizeof (int) * CHAR_BIT;
  int i7 : sizeof (int) * CHAR_BIT;
  int i8 : sizeof (int) * CHAR_BIT;
};

int f[sizeof (struct s) != sizeof (int) * 8 ? -1 : 1];
