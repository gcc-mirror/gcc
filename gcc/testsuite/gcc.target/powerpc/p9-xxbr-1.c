/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O3" } */

#include <altivec.h>

/* Verify P9 vec_revb builtin generates the XXBR{Q,D,W,H} instructions.  */

vector char
rev_char (vector char a)
{
  return vec_revb (a);		/* XXBRQ.  */
}

vector signed char
rev_schar (vector signed char a)
{
  return vec_revb (a);		/* XXBRQ.  */
}

vector unsigned char
rev_uchar (vector unsigned char a)
{
  return vec_revb (a);		/* XXBRQ.  */
}

vector short
rev_short (vector short a)
{
  return vec_revb (a);		/* XXBRH.  */
}

vector unsigned short
rev_ushort (vector unsigned short a)
{
  return vec_revb (a);		/* XXBRH.  */
}

vector int
rev_int (vector int a)
{
  return vec_revb (a);		/* XXBRW.  */
}

vector unsigned int
rev_uint (vector unsigned int a)
{
  return vec_revb (a);		/* XXBRW.  */
}

vector float
rev_float (vector float a)
{
  return vec_revb (a);		/* XXBRW.  */
}

vector double
rev_double (vector double a)
{
  return vec_revb (a);		/* XXBRD.  */
}

/* { dg-final { scan-assembler-times "xxbrd" 1 } } */
/* { dg-final { scan-assembler-times "xxbrh" 2 } } */
/* { dg-final { scan-assembler-times "xxbrq" 3 } } */
/* { dg-final { scan-assembler-times "xxbrw" 3 } } */
