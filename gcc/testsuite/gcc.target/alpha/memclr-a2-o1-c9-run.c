/* { dg-do run } */
/* { dg-options "" } */

#include "memclr-a2-o1-c9-ptr.c"

u_t u = { { { [0] = 0xaa }, {{ [0 ... 8] = 0xaa }}, { [0 ... 5] = 0xaa } } };

int
main (void)
{
  int i;

  memclr_a2_o1_c9 (&u);
  asm ("" : : : "memory");
  for (i = 0; i < sizeof (u.c); i++)
    if (u.c[i] != 0xaa)
      __builtin_abort ();
  for (i = 0; i < sizeof (u.s.v); i++)
    if (u.s.v[i] != 0x00)
      __builtin_abort ();
  for (i = 0; i < sizeof (u.d); i++)
    if (u.d[i] != 0xaa)
      __builtin_abort ();
  return 0;
}
