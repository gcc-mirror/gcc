/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O3" } */

#include <altivec.h>

/* Verify P9 vec_revb builtin generates the XXBR{Q,D,W,H} instructions.  */

vector char
rev_char (vector char a)
{
  return vec_revb (a);		/* Is a NOP, maps to move inst  */
}

vector bool char
rev_bool_char (vector bool char a)
{
  return vec_revb (a);		/* Is a NOP, maps to move inst  */
}

vector signed char
rev_schar (vector signed char a)
{
  return vec_revb (a);		/* Is a NOP, maps to move inst  */
}

vector unsigned char
rev_uchar (vector unsigned char a)
{
  return vec_revb (a);		/* Is a NOP, maps to move inst  */
}

vector short
rev_short (vector short a)
{
  return vec_revb (a);		/* XXBRH.  */
}

vector bool short
rev_bool_short (vector bool short a)
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

vector bool int
rev_bool_int (vector bool int a)
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
/* { dg-final { scan-assembler-times "xxbrh" 3 } } */
/* { dg-final { scan-assembler-times "xxbrw" 4 } } */
