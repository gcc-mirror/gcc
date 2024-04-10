/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

#include <altivec.h>

/* Verify P9 vec_revb builtin generates the XXBR{Q,D,W,H} instructions.  This
   test only tests the vector types that need a 64-bit environment.  */

vector long
rev_long (vector long a)
{
  return vec_revb (a);		/* XXBRD.  */
}

vector unsigned long
rev_ulong (vector unsigned long a)
{
  return vec_revb (a);		/* XXBRD.  */
}

vector long long
rev_long_long (vector long long a)
{
  return vec_revb (a);		/* XXBRD.  */
}

vector bool long long
rev_bool_long_long (vector bool long long a)
{
  return vec_revb (a);		/* XXBRD.  */
}

vector unsigned long long
rev_ulong_ulong (vector unsigned long long a)
{
  return vec_revb (a);		/* XXBRD.  */
}

vector __int128_t
rev_int128 (vector __int128_t a)
{
  return vec_revb (a);		/* XXBRQ.  */
}

vector __uint128_t
rev_uint128 (vector __uint128_t a)
{
  return vec_revb (a);		/* XXBRQ.  */
}

/* { dg-final { scan-assembler-times "xxbrd" 5 } } */
/* { dg-final { scan-assembler-times "xxbrq" 2 } } */
