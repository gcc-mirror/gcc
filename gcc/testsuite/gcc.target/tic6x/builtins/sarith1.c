#include <c6x_intrinsics.h>

extern void abort (void);

int a1 = 0x50000000;
int b1 = 0xc0000000;
int c1 = 0x40000000;
int a2 = 0xd0000000;
int b2 = 0x20000000;
int c2 = 0x90000000;
int d = 0x80000000;

int main ()
{
  if (_sadd (a1, b1) != 0x10000000)
    abort ();
  if (_sadd (a2, b2) != 0xf0000000)
    abort ();
  if (_sadd (a1, c1) != 0x7fffffff)
    abort ();
  if (_sadd (a2, c2) != 0x80000000)
    abort ();

  if (_ssub (a1, b1) != 0x7fffffff)
    abort ();
  if (_ssub (a2, b2) != 0xb0000000)
    abort ();
  if (_ssub (b1, a1) != 0x80000000)
    abort ();
  if (_ssub (b2, a2) != 0x50000000)
    abort ();

  if (_abs (a1) != 0x50000000)
    abort ();
  if (_abs (b1) != 0x40000000)
    abort ();
  if (_abs (d) != 0x7fffffff)
    abort ();

  if (_sshl (a1, 1) != 0x7fffffff
      || _sshl (b2, 1) != 0x40000000
      || _sshl (a2, 1) != 0xa0000000
      || _sshl (a2, 4) != 0x80000000)
    abort ();

  return 0;
}
