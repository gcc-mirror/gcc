/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

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

/* Expected test results:

     test_ne_char              1 vcmpneb
     test_ne_short             1 vcmpneh
     test_ne_int               1 vcmpnew
     test_ne_long              1 vcmpequd, 1 xxlnor inst
     test_nabs_long_long       1 xxspltib, 1 vsubudm, 1 vminsd
     test_neg_long_long        1 vnegd
     test_vull_bperm_vull_vuc  1 vbpermd


/* { dg-final { scan-assembler-times "vcmpneb"  1 } } */
/* { dg-final { scan-assembler-times "vcmpneh"  1 } } */
/* { dg-final { scan-assembler-times "vcmpnew"  1 } } */
/* { dg-final { scan-assembler-times "vcmpequd" 1 } } */
/* { dg-final { scan-assembler-times "xxlnor"   1 } } */
/* { dg-final { scan-assembler-times "xxspltib" 1 } } */
/* { dg-final { scan-assembler-times "vsubudm"  1 } } */
/* { dg-final { scan-assembler-times "vminsd"   1 } } */
/* { dg-final { scan-assembler-times "vnegd"    1 } } */
/* { dg-final { scan-assembler-times "vbpermd"  1 } } */

