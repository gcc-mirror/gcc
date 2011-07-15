#include <c6x_intrinsics.h>
extern void abort (void);

int a1 = 0x50000000;
int b1 = 0xc0000000;
int a2 = 0xd0000000;
int b2 = 0x20000000;
int c = 0x80000000;
int main ()
{
  if (_smpyh (a1, b1) != 0xd8000000)
    abort ();
  if (_smpyh (a2, b2) != 0xf4000000)
    abort ();
  if (_smpyh (c, c) != 0x7fffffff)
    abort ();

  return 0;
}
