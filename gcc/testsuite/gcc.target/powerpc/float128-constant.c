/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the LXVKQ instruction is generated to load special IEEE 128-bit
   constants.  */

_Float128
return_0 (void)
{
  return 0.0f128;			/* XXSPLTIB 34,0.  */
}

_Float128
return_1 (void)
{
  return 1.0f128;			/* LXVKQ 34,1.  */
}

_Float128
return_2 (void)
{
  return 2.0f128;			/* LXVKQ 34,2.  */
}

_Float128
return_3 (void)
{
  return 3.0f128;			/* LXVKQ 34,3.  */
}

_Float128
return_4 (void)
{
  return 4.0f128;			/* LXVKQ 34,4.  */
}

_Float128
return_5 (void)
{
  return 5.0f128;			/* LXVKQ 34,5.  */
}

_Float128
return_6 (void)
{
  return 6.0f128;			/* LXVKQ 34,6.  */
}

_Float128
return_7 (void)
{
  return 7.0f128;			/* LXVKQ 34,7.  */
}

_Float128
return_m0 (void)
{
  return -0.0f128;			/* LXVKQ 34,16.  */
}

_Float128
return_m1 (void)
{
  return -1.0f128;			/* LXVKQ 34,17.  */
}

_Float128
return_m2 (void)
{
  return -2.0f128;			/* LXVKQ 34,18.  */
}

_Float128
return_m3 (void)
{
  return -3.0f128;			/* LXVKQ 34,19.  */
}

_Float128
return_m4 (void)
{
  return -4.0f128;			/* LXVKQ 34,20.  */
}

_Float128
return_m5 (void)
{
  return -5.0f128;			/* LXVKQ 34,21.  */
}

_Float128
return_m6 (void)
{
  return -6.0f128;			/* LXVKQ 34,22.  */
}

_Float128
return_m7 (void)
{
  return -7.0f128;			/* LXVKQ 34,23.  */
}

_Float128
return_inf (void)
{
  return __builtin_inff128 ();		/* LXVKQ 34,8.  */
}

_Float128
return_minf (void)
{
  return - __builtin_inff128 ();	/* LXVKQ 34,24.  */
}

_Float128
return_nan (void)
{
  return __builtin_nanf128 ("");	/* LXVKQ 34,9.  */
}

/* Note, the following NaNs should not generate a LXVKQ instruction.  */
_Float128
return_mnan (void)
{
  return - __builtin_nanf128 ("");	/* PLXV 34,... */
}

_Float128
return_nan2 (void)
{
  return __builtin_nanf128 ("1");	/* PLXV 34,... */
}

_Float128
return_nans (void)
{
  return __builtin_nansf128 ("");	/* PLXV 34,... */
}

vector long long
return_longlong_neg_0 (void)
{
  /* This vector is the same pattern as -0.0F128.  */
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define FIRST	0x8000000000000000
#define SECOND	0x0000000000000000

#else
#define FIRST	0x0000000000000000
#define SECOND	0x8000000000000000
#endif

  return (vector long long) { FIRST, SECOND };	/* LXVKQ 34,16.  */
}

/* { dg-final { scan-assembler-times {\mlxvkq\M}    19 } } */
/* { dg-final { scan-assembler-times {\mplxv\M}      3 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M}  1 } } */

