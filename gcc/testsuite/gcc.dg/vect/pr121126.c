/* { dg-additional-options "--param vect-partial-vector-usage=2" } */

#include "tree-vect.h"

unsigned char a;
unsigned b;
int r[11];
static void __attribute__((noipa))
c(int e, unsigned s[][11][11])
{
  for (int u = -(e ? 2000424973 : 0) - 2294542319; u < 7; u += 4)
    for (int x = 0; x < 300000011; x += 4)
      for (int y = 0; y < (0 < s[u][4][1]) + 11; y += 3) {
        a = a ?: 1;
        b = r[2];
      }
}
long long ab;
int e = 1;
unsigned s[11][11][11];
int main()
{
  check_vect ();
  for (int t = 0; t < 11; ++t)
    r[t] = 308100;
  c(e,s);
  ab = b;
  if (ab != 308100)
    __builtin_abort ();
}
