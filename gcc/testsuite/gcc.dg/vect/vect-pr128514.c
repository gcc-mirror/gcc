#include "tree-vect.h"

#pragma scalar_storage_order big-endian
struct S {
  unsigned b : 5;
};
#pragma scalar_storage_order default

struct S a[16];
unsigned char c[16][3];

int main()
{
  check_vect ();

  for (int i = 0; i < 16; i++)
    c[i][0] = i + 1;
  for (int i = 0; i < 16; i++)
    {
      c[i][1] = c[i][2] = 0;
      a[i ^ 1].b = c[i ^ 1][0];
    }
  if (a[15].b != 16)
    __builtin_abort ();
  return 0;
}
