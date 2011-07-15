#include <c6x_intrinsics.h>

extern void abort (void);

int a1 = 0x5000;
int b1 = 0xc0000000;
int a2 = 0xd000;
int b2 = 0x20000000;
int c = 0x8000;
int main ()
{
  if (_smpylh (a1, b1) != 0xd8000000)
    abort ();
  if (_smpylh (a2, b2) != 0xf4000000)
    abort ();
  if (_smpylh (c, 0x80000000) != 0x7fffffff)
    abort ();
  if (_smpyhl (b1, a1) != 0xd8000000)
    abort ();
  if (_smpyhl (b2, a2) != 0xf4000000)
    abort ();
  if (_smpyhl (0x80000000, c) != 0x7fffffff)
    abort ();

  return 0;
}
