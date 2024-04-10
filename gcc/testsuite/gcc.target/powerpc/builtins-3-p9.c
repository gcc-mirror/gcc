/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O1" } */

#include <altivec.h>

vector bool char
test_ne_char (vector bool char x, vector bool char y)
{
	return vec_cmpne (x, y);
}

vector bool short
test_ne_short (vector bool short x, vector bool short y)
{
	return vec_cmpne (x, y);
}

vector bool int
test_ne_int (vector bool int x, vector bool int y)
{
	return vec_cmpne (x, y);
}

vector bool long long
test_ne_long (vector bool long long x, vector bool long long y)
{
	return vec_cmpne (x, y);
}

vector long long
test_nabs_long_long (vector long long x)
{
  return vec_nabs (x);
}

vector long long
test_neg_long_long (vector long long x)
{
	return vec_neg (x);
}

vector unsigned long long
test_vull_bperm_vull_vuc (vector unsigned long long x,
                          vector unsigned char y)
{
	return vec_bperm (x, y);
}

vector signed char
test_nabs_char (vector signed char x)
{
	return vec_nabs (x);
}

vector short
test_nabs_short (vector short x)
{
  return vec_nabs (x);
}

vector int
test_nabs_int (vector int x)
{
  return vec_nabs (x);
}


vector signed char
test_neg_char (vector signed char x)
{
	return vec_neg (x);
}

vector short
test_neg_short (vector short x)
{
	return vec_neg (x);
}

vector int
test_neg_int (vector int x)
{
	return vec_neg (x);
}

/* Expected test results:

     test_ne_char              1 vcmpneb
     test_ne_short             1 vcmpneh
     test_ne_int               1 vcmpnew
     test_ne_long              1 vcmpequd, 1 xxlnor inst
     test_neg_long_long        1 vnegd
     test_vull_bperm_vull_vuc  1 vbpermd
     test_nabs_long_long (-O0) 1 xxspltib, 1 vsubudm, 1 vminsd
     test_nabs_long_long (-O1) 1 vnegd, vminsd
     test_nabs_char (P9)       1 xxspltib, 1 vsububm, 1 vminsb
     test_nabs_short (P9)      1 xxspltib, 1 vsubuhm, 1 vminsh
     test_nabs_int (P9)        1 vnegw, 1 vminsw
     test_neg_char (P9)        1 xxspltib, 1 vsububm
     test_neg_short (P9)       1 xxspltib, 1 vsubuhm
     test_neg_int (P9)         1 vnegw
*/

/* { dg-final { scan-assembler-times "vcmpneb"  1 } } */
/* { dg-final { scan-assembler-times "vcmpneh"  1 } } */
/* { dg-final { scan-assembler-times "vcmpnew"  1 } } */
/* { dg-final { scan-assembler-times "vcmpequd" 1 } } */
/* { dg-final { scan-assembler-times "xxlnor"   1 } } */
/* { dg-final { scan-assembler-times "xxspltib" 4 } } */
/* { dg-final { scan-assembler-times "vsubudm"  0 } } */
/* { dg-final { scan-assembler-times "vsububm"  2 } } */
/* { dg-final { scan-assembler-times "vsubuhm"  2 } } */
/* { dg-final { scan-assembler-times "vsubuwm"  0 } } */
/* { dg-final { scan-assembler-times "vminsb"   1 } } */
/* { dg-final { scan-assembler-times "vminsh"   1 } } */
/* { dg-final { scan-assembler-times "vminsw"   1 } } */
/* { dg-final { scan-assembler-times "vminsd"   1 } } */
/* { dg-final { scan-assembler-times "vnegd"    2 } } */
/* { dg-final { scan-assembler-times "vnegw"    2 } } */
/* { dg-final { scan-assembler-times "vbpermd"  1 } } */

