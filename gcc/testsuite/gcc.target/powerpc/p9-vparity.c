/* { dg-do compile { target lp64 } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

#include <altivec.h>

vector int
parity_v4si_1s (vector int a)
{
  return vec_vprtyb (a);
}

vector int
parity_v4si_2s (vector int a)
{
  return vec_vprtybw (a);
}

vector unsigned int
parity_v4si_1u (vector unsigned int a)
{
  return vec_vprtyb (a);
}

vector unsigned int
parity_v4si_2u (vector unsigned int a)
{
  return vec_vprtybw (a);
}

vector long long
parity_v2di_1s (vector long long a)
{
  return vec_vprtyb (a);
}

vector long long
parity_v2di_2s (vector long long a)
{
  return vec_vprtybd (a);
}

vector unsigned long long
parity_v2di_1u (vector unsigned long long a)
{
  return vec_vprtyb (a);
}

vector unsigned long long
parity_v2di_2u (vector unsigned long long a)
{
  return vec_vprtybd (a);
}

vector __int128_t
parity_v1ti_1s (vector __int128_t a)
{
  return vec_vprtyb (a);
}

vector __int128_t
parity_v1ti_2s (vector __int128_t a)
{
  return vec_vprtybq (a);
}

__int128_t
parity_ti_3s (__int128_t a)
{
  return vec_vprtyb (a);
}

__int128_t
parity_ti_4s (__int128_t a)
{
  return vec_vprtybq (a);
}

vector __uint128_t
parity_v1ti_1u (vector __uint128_t a)
{
  return vec_vprtyb (a);
}

vector __uint128_t
parity_v1ti_2u (vector __uint128_t a)
{
  return vec_vprtybq (a);
}

__uint128_t
parity_ti_3u (__uint128_t a)
{
  return vec_vprtyb (a);
}

__uint128_t
parity_ti_4u (__uint128_t a)
{
  return vec_vprtybq (a);
}

/* { dg-final { scan-assembler "vprtybd" } } */
/* { dg-final { scan-assembler "vprtybq" } } */
/* { dg-final { scan-assembler "vprtybw" } } */
/* { dg-final { scan-assembler-not "vpopcntb" } } */
