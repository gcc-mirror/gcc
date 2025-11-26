/* { dg-do run { target bitint575 } } */
/* { dg-options "-std=gnu2y" } */

#define assert(e)  ((e) ? (void) 0 : __builtin_abort ())

void limits (void);

int
main (void)
{
  limits ();
}

void
limits (void)
{
  unsigned _BitInt (500) u;
  _BitInt (500) i;

  u = 0;
  u--;

  assert (_Maxof (unsigned _BitInt (500)) == u);
  assert (_Minof (unsigned _BitInt (500)) == 0);

  i = u >> 1;

  assert (_Maxof (_BitInt (500)) == i);
  assert (_Minof (_BitInt (500)) == -i-1);
}

void
type (void)
{
  _Generic (_Maxof (_BitInt (500)), _BitInt (500): 0);
  _Generic (_Minof (_BitInt (500)), _BitInt (500): 0);
  _Generic (_Maxof (unsigned _BitInt (500)), unsigned _BitInt (500): 0);
  _Generic (_Minof (unsigned _BitInt (500)), unsigned _BitInt (500): 0);
}
