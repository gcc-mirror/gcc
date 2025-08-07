/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */

/* Verify calling convention. */

static_assert (sizeof (_BitInt(65)) == 16);

[[gnu::noipa]] void
bitint65_zero_extend (unsigned _BitInt(65) x)
{
  static const char y[16] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
                             0xBA, 0xDC, 0x0F, 0xFE, 0xE0, 0xDD, 0xF0, 0x0D};
  if (__builtin_memcmp (&x, y, 16) != 0)
    __builtin_abort ();
}

[[gnu::noipa]] void
bitint65_sign_extend (signed _BitInt(65) x)
{
  static const char y[16] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
                             0xBA, 0xDC, 0x0F, 0xFE, 0xE0, 0xDD, 0xF0, 0x0D};
  if (__builtin_memcmp (&x, y, 16) != 0)
    __builtin_abort ();
}

int
main (void)
{
  bitint65_zero_extend (0x1BADC0FFEE0DDF00Dwbu);
  bitint65_sign_extend (0x1BADC0FFEE0DDF00Dwb);
  return 0;
}
