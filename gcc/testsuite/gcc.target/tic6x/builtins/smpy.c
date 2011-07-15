#include <c6x_intrinsics.h>

extern void abort (void);

int a1 = 0x5000;
int b1 = 0xc000;
int a2 = 0xd000;
int b2 = 0x2000;
int c = 0x8000;
int main ()
{
  if (_smpy (a1, b1) != 0xd8000000)
    abort ();
  if (_smpy (a2, b2) != 0xf4000000)
    abort ();
  if (_smpy (c, c) != 0x7fffffff)
    abort ();

  return 0;
}
