/* PR target/112672 */
/* { dg-do run } */
/* { dg-options "-O2" } */

typedef unsigned short u16;

u16 g = 254;

static inline u16
foo (u16 u)
{
  u *= g;
  return u + __builtin_parityl (u);
}

int
main (void)
{
  u16 x = foo (4);
  if (x != 4 * 254 + 1)
    __builtin_abort ();
  return 0;
}
