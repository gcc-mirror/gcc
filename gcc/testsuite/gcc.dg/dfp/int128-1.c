/* PR libgcc/65833 */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target bitint } */
/* { dg-options "-O2 -std=gnu2x" } */

#define INT128_MAX ((__int128) ((((unsigned __int128) 1) << 127) - 1))
#define UINT128_MAX (~(unsigned __int128) 0)
#define C(x, y) ((((__int128) (x##ULL)) << 64) | (y##ULL))
#define UC(x, y) ((((unsigned __int128) (x##ULL)) << 64) | (y##ULL))

__attribute__((noipa)) __int128
tests64 (_Decimal64 d)
{
  return d;
}

__attribute__((noipa)) unsigned __int128
testu64 (_Decimal64 d)
{
  return d;
}

__attribute__((noipa)) __int128
tests32 (_Decimal32 d)
{
  return d;
}

__attribute__((noipa)) unsigned __int128
testu32 (_Decimal32 d)
{
  return d;
}

__attribute__((noipa)) __int128
tests128 (_Decimal128 d)
{
  return d;
}

__attribute__((noipa)) unsigned __int128
testu128 (_Decimal128 d)
{
  return d;
}

int
main ()
{
  if (tests64 (0.DD) != 0
      || tests64 (0.9999999999999999DD) != 0
      || tests64 (7.999999999999999DD) != 7
      || tests64 (-0.DD) != 0
      || tests64 (-0.9999999999999999DD) != 0
      || tests64 (-42.5DD) != -42
      || tests64 (-34242319854.45429e+27DD) != -C (0x19c2d4b6fefc3378, 0xa349b93967400000)
      || tests64 (-213855087769445.9e+23DD) != -C (0x1016b2fcff8f2cf6, 0xd16cf61904c00000)
      || tests64 (1701411834604692.0e+23DD) != C (0x7ffffffffffff947, 0xd26076f482000000)
      || tests64 (-1701411834604692.0e+23DD) != -C (0x7ffffffffffff947, 0xd26076f482000000))
    __builtin_abort ();
  if (tests64 (1701411834604693.0e+23DD) != INT128_MAX
      || tests64 (9999999999999999e+369DD) != INT128_MAX
      || tests64 (-1701411834604693.0e+23DD) != -INT128_MAX - 1
      || tests64 (-9999999999999999e+369DD) != -INT128_MAX - 1)
    __builtin_abort ();
  if (testu64 (0.DD) != 0
      || testu64 (0.9999999999999999DD) != 0
      || testu64 (-0.9999999999999999DD) != 0
      || testu64 (-0.0DD) != 0
      || testu64 (-0.5DD) != 0
      || testu64 (42.99999999999999DD) != 42
      || testu64 (42.e+21DD) != UC (0x8e4, 0xd316827686400000)
      || testu64 (34272319854.45429e+27DD) != C (0x19c89bd43b04cab9, 0x49f2646567400000)
      || testu64 (3402823669209384.0e+23DD) != C (0xfffffffffffff28f, 0xa4c0ede904000000))
    __builtin_abort ();
  if (testu64 (-1.DD) != 0
      || testu64 (-42.5e+15DD) != 0
      || testu64 (-9999999999999999e+369DD) != 0
      || testu64 (3402823669209385.0e+23DD) != UINT128_MAX
      || testu64 (9999999999999999e+369DD) != UINT128_MAX)
    __builtin_abort ();

  if (tests32 (0.DF) != 0
      || tests32 (0.9999999DF) != 0
      || tests32 (7.999999DF) != 7
      || tests32 (-0.000DF) != 0
      || tests32 (-0.9999999DF) != 0
      || tests32 (-1.DF) != -1
      || tests32 (-42.5DF) != -42
      || tests32 (-3424.231e+27DF) != -C (0x2b38497f00, 0x9c4e190b47000000)
      || tests32 (-213855.9e+32DF) != -C (0x1016b6fe2d67e732, 0x717a483980000000)
      || tests32 (1701411.0e+32DF) != C (0x7ffffbe294adefda, 0xd863b4a300000000)
      || tests32 (-1701411.0e+32DF) != -C (0x7ffffbe294adefda, 0xd863b4a300000000))
    __builtin_abort ();
  if (tests32 (1701412.0e+32DF) != INT128_MAX
      || tests32 (9999999e+90DF) != INT128_MAX
      || tests32 (-1701412.0e+32DF) != -INT128_MAX - 1
      || tests32 (-9999999e+90DF) != -INT128_MAX - 1)
    __builtin_abort ();
  if (testu32 (0.DF) != 0
      || testu32 (0.9999999DF) != 0
      || testu32 (-0.9999999DF) != 0
      || testu32 (-0.5DF) != 0
      || testu32 (-0.0000DF) != 0
      || testu32 (-0.99999DF) != 0
      || testu32 (42.99999DF) != 42
      || testu32 (42.e+21DF) != UC (0x8e4, 0xd316827686400000)
      || testu32 (3402.823e+35DF) != UC (0xfffffcb356c92111, 0x367458c700000000))
    __builtin_abort ();
  if (testu32 (-1.DF) != 0
      || testu32 (-42.5e+15DF) != 0
      || testu32 (-9999999e+90DF) != 0
      || testu32 (3402.824e+35DF) != UINT128_MAX
      || testu32 (9999999e+90DF) != UINT128_MAX)
    __builtin_abort ();

  if (tests128 (0.DL) != 0
      || tests128 (0.9999999999999999999999999999999999DL) != 0
      || tests128 (7.999999999999999999999999999999999DL) != 7
      || tests128 (-0.DL) != 0
      || tests128 (-0.9999999999999999999999999999999999DL) != 0
      || tests128 (-1.DL) != -1
      || tests128 (-42.5DL) != -42
      || tests128 (-34242319854.45429439857871298745432e+27DL) != -C (0x19c2d4b6fefc3467, 0x15d47c047b56ad80)
      || tests128 (-213855087769445.9e+23DL) != -C (0x1016b2fcff8f2cf6, 0xd16cf61904c00000)
      || tests128 (1701411834604692317316873037158841.0e+5DL) != C (0x7fffffffffffffff, 0xffffffffffffe9a0)
      || tests128 (-1701411834604692317316873037158841.0e+5DL) != -C (0x7fffffffffffffff, 0xffffffffffffe9a0))
    __builtin_abort ();
  if (tests128 (1701411834604692317316873037158842.0e+5DL) != INT128_MAX
      || tests128 (9999999999999999999999999999999999e+6111DL) != INT128_MAX
      || tests128 (-1701411834604692317316873037158842.0e+5DL) != -INT128_MAX - 1
      || tests128 (-9999999999999999999999999999999999e+6111DL) != -INT128_MAX - 1)
    __builtin_abort ();
  if (testu128 (0.DL) != 0
      || testu128 (0.9999999999999999999999999999999999DL) != 0
      || testu128 (-0.9999999999999999999999999999999999DL) != 0
      || testu128 (-0.DL) != 0
      || testu128 (-0.9999999999999999999999DL) != 0
      || testu128 (-0.5DL) != 0
      || testu128 (42.99999999999999999999999999999999DL) != 42
      || testu128 (42.e+21DL) != UC (0x8e4, 0xd316827686400000)
      || testu128 (34242319854.45429439857871298745432e+21DL) != UC (0x1b032e71cc9, 0x24b5cd6d86a9473e)
      || testu128 (3402823669209384634633746074317.682e+8DL) != UC (0xffffffffffffffff, 0xffffffffffffd340))
    __builtin_abort ();
  if (testu128 (-1.DL) != 0
      || testu128 (-42.5e+15DL) != 0
      || testu128 (-9999999999999999999999999999999999e+6111DL) != 0
      || testu128 (3402823669209384634633746074317.683e+8DL) != UINT128_MAX
      || testu128 (9999999999999999999999999999999999e+6111DL) != UINT128_MAX)
    __builtin_abort ();
}
