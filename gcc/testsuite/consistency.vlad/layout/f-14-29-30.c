#include <stdio.h>

struct sss{
  int i1:14;
  int i2:29;
  int i3:30;
};

static union u{
  struct sss sss;
  unsigned char a[sizeof (struct sss)];
} u;

int main (void) {
  int i;
  for (i = 0; i < sizeof (struct sss); i++)
    u.a[i] = 0;
  u.sss.i1 = 16383.0;
  for (i = 0; i < sizeof (struct sss); i++)
    printf ("%x ", u.a[i]);
  printf ("\n");
  u.sss.i2 = 536870911.0;
  for (i = 0; i < sizeof (struct sss); i++)
    printf ("%x ", u.a[i]);
  printf ("\n");
  u.sss.i3 = 1073741823.0;
  for (i = 0; i < sizeof (struct sss); i++)
    printf ("%x ", u.a[i]);
  printf ("\n");
  return 0;
}
