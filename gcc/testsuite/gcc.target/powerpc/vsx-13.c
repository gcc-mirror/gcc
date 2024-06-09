/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Variations of tests that require VSX support.  This is a variation of
   the altivec-13.c testcase.  */

#include <altivec.h>

void foo (void)
{

  vector signed char vsc1, vsc2, vscz;
  vector unsigned char vuc1, vuc2, vucz;
  vector bool long long vubll1, vubll2, vubllz;
  vector signed int long long vsill1, vsill2, vsillz;
  vector unsigned int long long vuill1, vuill2, vuillz;
  vector double vd1, vd2, vdz;

  vubllz = vec_sld( vubll1, vubll2, 1 );
  vsillz = vec_sld( vsill1, vsill2, 1 );
  vuillz = vec_sld( vuill1, vuill2, 1 );

  vsillz = vec_srl(vsill1, vuc2);
  vuillz = vec_srl(vuill1, vuc2);

  vsillz = vec_sro(vsill1, vsc2);
  vsillz = vec_sro(vsill1, vuc2);
  vuillz = vec_sro(vuill1, vsc2);
  vuillz = vec_sro(vuill1, vuc2);

  vdz = vec_sld( vd1, vd2, 1 );
}

/* Expected results:
   vec_sld          vsldoi
   vec_srl          vsr
   vec_sro          vsro  */

/* { dg-final { scan-assembler-times "vsldoi" 4 } } */
/* { dg-final { scan-assembler-times "vsr " 2 } } */
/* { dg-final { scan-assembler-times "vsro" 4 } } */
