/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

/* Test POWER8 vector built-ins added for version 1.1 of ELFv2 ABI.  */

vector unsigned char      vuca, vucb, vucc;
vector bool     char      vbca, vbcb;
vector bool     short     vbsa, vbsb;
vector bool     int       vbia, vbib;
vector signed   long long vsla, vslb;
vector unsigned long long vula, vulb, vulc;
vector bool     long long vbla, vblb, vblc;
vector signed   __int128  vsxa, vsxb, vsxc;
vector unsigned __int128  vuxa, vuxb, vuxc;
vector          double    vda,  vdb;

void foo (vector unsigned char *vucr,
	  vector bool char *vbcr,
	  vector bool short *vbsr,
	  vector bool int *vbir,
	  vector unsigned long long *vulr,
	  vector bool long long *vblr,
	  vector signed __int128 *vsxr,
	  vector unsigned __int128 *vuxr,
	  vector double *vdr)
{
  *vsxr++ = vec_addc (vsxa, vsxb);
  *vuxr++ = vec_addc (vuxa, vuxb);
  *vsxr++ = vec_adde (vsxa, vsxb, vsxc);
  *vuxr++ = vec_adde (vuxa, vuxb, vuxc);
  *vsxr++ = vec_addec (vsxa, vsxb, vsxc);
  *vuxr++ = vec_addec (vuxa, vuxb, vuxc);
  *vulr++ = vec_bperm (vuxa, vucb);
  *vbcr++ = vec_eqv (vbca, vbcb);
  *vbir++ = vec_eqv (vbia, vbib);
  *vblr++ = vec_eqv (vbla, vblb);
  *vbsr++ = vec_eqv (vbsa, vbsb);
  *vucr++ = vec_gb (vuca);
  *vbcr++ = vec_nand (vbca, vbcb);
  *vbir++ = vec_nand (vbia, vbib);
  *vblr++ = vec_nand (vbla, vblb);
  *vbsr++ = vec_nand (vbsa, vbsb);
  *vbcr++ = vec_orc (vbca, vbcb);
  *vbir++ = vec_orc (vbia, vbib);
  *vblr++ = vec_orc (vbla, vblb);
  *vbsr++ = vec_orc (vbsa, vbsb);
  *vblr++ = vec_perm (vbla, vblb, vucc);
}

/* { dg-final { scan-assembler-times "vaddcuq" 2 } } */
/* { dg-final { scan-assembler-times "vaddeuqm" 2 } } */
/* { dg-final { scan-assembler-times "vaddecuq" 2 } } */
/* { dg-final { scan-assembler-times "vbpermq" 1 } } */
/* { dg-final { scan-assembler-times "xxleqv" 4 } } */
/* { dg-final { scan-assembler-times "vgbbd" 1 } } */
/* { dg-final { scan-assembler-times "xxlnand" 4 } } */
/* { dg-final { scan-assembler-times "xxlorc" 4 } } */
/* { dg-final { scan-assembler-times "vperm" 1 } } */

