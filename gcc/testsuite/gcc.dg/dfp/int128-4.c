/* PR libgcc/65833 */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target bitint } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "-std=c2x" } */

#include <fenv.h>

#define INT128_MAX ((__int128) ((((unsigned __int128) 1) << 127) - 1))
#define UINT128_MAX (~(unsigned __int128) 0)
#define C(x, y) ((((__int128) (x##ULL)) << 64) | (y##ULL))
#define UC(x, y) ((((unsigned __int128) (x##ULL)) << 64) | (y##ULL))

__attribute__((noipa)) __int128
tests_32 (_Decimal32 d)
{
  return d;
}

__attribute__((noipa)) unsigned __int128
testu_32 (_Decimal32 d)
{
  return d;
}

__attribute__((noipa)) __int128
tests_64 (_Decimal64 d)
{
  return d;
}

__attribute__((noipa)) unsigned __int128
testu_64 (_Decimal64 d)
{
  return d;
}

__attribute__((noipa)) __int128
tests_128 (_Decimal128 d)
{
  return d;
}

__attribute__((noipa)) unsigned __int128
testu_128 (_Decimal128 d)
{
  return d;
}

__attribute__((noipa)) void
check_invalid (int test, int inv)
{
  if (!test)
    __builtin_abort ();
  if ((!fetestexcept (FE_INVALID)) != (!inv))
    __builtin_abort ();
  feclearexcept (FE_INVALID);
}

int
main ()
{
  check_invalid (tests_32 (__builtin_infd32 ()) == INT128_MAX, 1);
  check_invalid (tests_32 (-__builtin_infd32 ()) == -INT128_MAX - 1, 1);
  check_invalid (tests_32 (__builtin_nand32 ("")) == INT128_MAX, 1);
  check_invalid (tests_32 (-1701411.0e+32DF) == -C (0x7ffffbe294adefda, 0xd863b4a300000000), 0);
  check_invalid (tests_32 (-1701412.0e+32DF) == -INT128_MAX - 1, 1);
  check_invalid (tests_32 (1701411.0e+32DF) == C (0x7ffffbe294adefda, 0xd863b4a300000000), 0);
  check_invalid (tests_32 (1701412.0e+32DF) == INT128_MAX, 1);
  check_invalid (testu_32 (__builtin_infd32 ()) == UINT128_MAX, 1);
  check_invalid (testu_32 (-__builtin_infd32 ()) == 0U, 1);
  check_invalid (testu_32 (__builtin_nand32 ("")) == UINT128_MAX, 1);
  check_invalid (testu_32 (-0.9999999DF) == 0U, 0);
  check_invalid (testu_32 (-1.0DF) == 0U, 1);
  check_invalid (testu_32 (3402823.0e+32DF) == UC (0xfffffcb356c92111, 0x367458c700000000), 0);
  check_invalid (testu_32 (3402824.0e+32DF) == UINT128_MAX, 1);
  check_invalid (tests_64 (__builtin_infd64 ()) == INT128_MAX, 1);
  check_invalid (tests_64 (-__builtin_infd64 ()) == -INT128_MAX - 1, 1);
  check_invalid (tests_64 (__builtin_nand64 ("")) == INT128_MAX, 1);
  check_invalid (tests_64 (-170141183460469.2e+24DD) == -C (0x7ffffffffffff947, 0xd26076f482000000), 0);
  check_invalid (tests_64 (-170141183460469.3e+24DD) == -INT128_MAX - 1, 1);
  check_invalid (tests_64 (170141183460469.2e+24DD) == C (0x7ffffffffffff947, 0xd26076f482000000), 0);
  check_invalid (tests_64 (170141183460469.3e+24DD) == INT128_MAX, 1);
  check_invalid (testu_64 (__builtin_infd64 ()) == UINT128_MAX, 1);
  check_invalid (testu_64 (-__builtin_infd64 ()) == 0, 1);
  check_invalid (testu_64 (__builtin_nand64 ("")) == UINT128_MAX, 1);
  check_invalid (testu_64 (-0.9999999999999999DD) == 0U, 0);
  check_invalid (testu_64 (-1.0DD) == 0U, 1);
  check_invalid (testu_64 (340282366920938.4e+24DD) == UC (0xfffffffffffff28f, 0xa4c0ede904000000), 0);
  check_invalid (testu_64 (340282366920938.5e+24DD) == UINT128_MAX, 1);
  check_invalid (tests_128 (__builtin_infd128 ()) == INT128_MAX, 1);
  check_invalid (tests_128 (-__builtin_infd128 ()) == -INT128_MAX - 1, 1);
  check_invalid (tests_128 (__builtin_nand128 ("")) == INT128_MAX, 1);
  check_invalid (tests_128 (-1701411834604692317316873037158841.0e+5DL) == -C (0x7fffffffffffffff, 0xffffffffffffe9a0), 0);
  check_invalid (tests_128 (-1701411834604692317316873037158842.0e+5DL) == -INT128_MAX - 1, 1);
  check_invalid (tests_128 (1701411834604692317316873037158841.0e+5DL) == C (0x7fffffffffffffff, 0xffffffffffffe9a0), 0);
  check_invalid (tests_128 (1701411834604692317316873037158842.0e+5DL) == INT128_MAX, 1);
  check_invalid (testu_128 (__builtin_infd128 ()) == UINT128_MAX, 1);
  check_invalid (testu_128 (-__builtin_infd128 ()) == 0, 1);
  check_invalid (testu_128 (__builtin_nand128 ("")) == UINT128_MAX, 1);
  check_invalid (testu_128 (-0.9999999999999999999999999999999999DL) == 0U, 0);
  check_invalid (testu_128 (-1.0DL) == 0U, 1);
  check_invalid (testu_128 (3402823669209384634633746074317682.0e+5DL) == UC (0xffffffffffffffff, 0xffffffffffffd340), 0);
  check_invalid (testu_128 (3402823669209384634633746074317683.0e+5DL) == UINT128_MAX, 1);
}
