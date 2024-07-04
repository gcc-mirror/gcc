
/* { dg-do compile } */
/* { dg-options "-maltivec -mno-vsx -O0" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

/* Test Altivec built-ins added for version 1.1 of ELFv2 ABI.  */

vector signed int vsia, vsib;
vector signed short vssa, vssb, vssc;
vector unsigned short vusa, vusb, vusc;

void foo (vector signed int *vsir,
	  vector signed short *vssr,
	  vector unsigned short *vusr)
{
  *vsir++ = vec_addc (vsia, vsib);
  *vssr++ = vec_madd (vssa, vssb, vssc);
  *vssr++ = vec_madd (vssa, vusb, vusc);
  *vssr++ = vec_madd (vusa, vssb, vssc);
  *vusr++ = vec_madd (vusa, vusb, vusc);

  *vssr++ = vec_madds (vssa, vssb, vssc);
}

/* { dg-final { scan-assembler-times "vaddcuw" 1 } } */
/* { dg-final { scan-assembler-times "vmladduhm" 4 } } */
/* { dg-final { scan-assembler-times "vmhaddshs" 1 } } */
