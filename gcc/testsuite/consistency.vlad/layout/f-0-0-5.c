#include <stdio.h>

struct sss{
  int :0;
  int :0;
  int i3:5;
};

static union u{
  struct sss sss;
  unsigned char a[sizeof (struct sss)];
} u;

int main (void) {
  int i;
  for (i = 0; i < sizeof (struct sss); i++)
    u.a[i] = 0;
  u.sss.i3 = 31.0;
  for (i = 0; i < sizeof (struct sss); i++)
    printf ("%x ", u.a[i]);
  printf ("\n");
  return 0;
}
