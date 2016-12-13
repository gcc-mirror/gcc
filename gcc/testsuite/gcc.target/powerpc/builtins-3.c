/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mvsx" } */

#include <altivec.h>

vector bool char
test_eq_char (vector bool char x, vector bool char y)
{
	return vec_cmpeq (x, y);
}

vector bool short
test_eq_short (vector bool short x, vector bool short y)
{
	return vec_cmpeq (x, y);
}

vector bool int
test_eq_int (vector bool int x, vector bool int y)
{
	return vec_cmpeq (x, y);
}

vector double
test_shift_left_double (vector double x, vector double y)
{
	return vec_sld (x, y, /* shift_by */ 10);
}

/* Expected test results:

     test_eq_char              1 vcmpequb inst
     test_eq_short             1 vcmpequh inst
     test_eq_int               1 vcmpequw inst
     test_shift_left_double    1 vsldoi inst */

/* { dg-final { scan-assembler-times "vcmpequb" 1 } } */
/* { dg-final { scan-assembler-times "vcmpequh" 1 } } */
/* { dg-final { scan-assembler-times "vcmpequw" 1 } } */
/* { dg-final { scan-assembler-times "vsldoi"   1 } } */
