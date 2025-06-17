/* { dg-do run } */

#include <string.h>

typedef int v4 __attribute__((vector_size (4 * sizeof (int))));

void __attribute__((noclone,noinline)) do_shift (v4 *vec, int shift)
{
  v4 t = *vec;

  if (shift > 0)
  {
    t = t >> shift;
  }

  *vec = t;
}

int main ()
{
#if __SIZEOF_INT__ >= 4
  v4 vec =  {0x1000000, 0x2000, 0x300, 0x40};
  v4 vec2 = {0x100000,  0x200,  0x30,  0x4};
#else
  v4 vec =  {0x4000, 0x2000, 0x300, 0x40};
  v4 vec2 = {0x400,  0x200,  0x30,  0x4};
#endif
  do_shift (&vec, 4);
  if (memcmp (&vec, &vec2, sizeof (v4)) != 0)
    __builtin_abort ();
  return 0;
}
