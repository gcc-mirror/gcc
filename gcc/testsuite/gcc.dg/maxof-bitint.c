/* { dg-do compile { target bitint } } */
/* { dg-options "-std=gnu2y" } */

void
limits (void)
{
  _Static_assert (_Maxof (_BitInt (5)) == 15);
  _Static_assert (_Minof (_BitInt (5)) == -16);
  _Static_assert (_Maxof (unsigned _BitInt (5)) == 31);
  _Static_assert (_Minof (unsigned _BitInt (5)) == 0);
}

void
type (void)
{
  _Generic (_Maxof (_BitInt (5)), _BitInt (5): 0);
  _Generic (_Minof (_BitInt (5)), _BitInt (5): 0);
  _Generic (_Maxof (unsigned _BitInt (5)), unsigned _BitInt (5): 0);
  _Generic (_Minof (unsigned _BitInt (5)), unsigned _BitInt (5): 0);
}
