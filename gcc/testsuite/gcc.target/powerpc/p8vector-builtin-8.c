/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target int128 } */

#include <altivec.h>

/* Test POWER8 vector built-ins added for version 1.1 of ELFv2 ABI.  */

vector signed char        vsca, vscb, vscc;
vector unsigned char      vuca, vucb, vucc;
vector signed short       vssa, vssb;
vector bool     char      vbca, vbcb;
vector unsigned short     vusa, vusb;
vector bool     short     vbsa, vbsb;
vector signed int         vsia, vsib, vsic;
vector unsigned int       vuia, vuib, vuic;
vector bool     int       vbia, vbib;
vector signed   long long vsla, vslb;
vector unsigned long long vula, vulb, vulc;
vector bool     long long vbla, vblb, vblc;
vector signed   __int128  vsxa, vsxb, vsxc;
vector unsigned __int128  vuxa, vuxb, vuxc;
vector          double    vda,  vdb;

void foo (vector signed char *vscr,
	  vector unsigned char *vucr,
	  vector bool char *vbcr,
	  vector signed short *vssr,
	  vector unsigned short *vusr,
	  vector bool short *vbsr,
	  vector signed int *vsir,
	  vector unsigned int *vuir,
	  vector bool int *vbir,
	  vector unsigned long long *vulr,
	  vector bool long long *vblr,
	  vector signed __int128 *vsxr,
	  vector unsigned __int128 *vuxr,
	  vector double *vdr)
{
  *vsir++ = vec_addc (vsia, vsib);
  *vuir++ = vec_addc (vuia, vuib);
  *vsxr++ = vec_addc (vsxa, vsxb);
  *vuxr++ = vec_addc (vuxa, vuxb);
  *vsir++ = vec_adde (vsia, vsib, vsic);
  *vuir++ = vec_adde (vuia, vuib, vuic);
  *vsxr++ = vec_adde (vsxa, vsxb, vsxc);
  *vuxr++ = vec_adde (vuxa, vuxb, vuxc);
  *vsir++ = vec_addec (vsia, vsib, vsic);
  *vuir++ = vec_addec (vuia, vuib, vuic);
  *vsxr++ = vec_addec (vsxa, vsxb, vsxc);
  *vuxr++ = vec_addec (vuxa, vuxb, vuxc);
  *vscr++ = vec_adds (vsca, vscb);
  *vucr++ = vec_adds (vuca, vucb);
  *vsir++ = vec_adds (vsia, vsib);
  *vuir++ = vec_adds (vuia, vuib);
  *vssr++ = vec_adds (vssa, vssb);
  *vusr++ = vec_adds (vusa, vusb);
  *vscr++ = vec_adds (vbca, vscb);
  *vscr++ = vec_adds (vsca, vbcb);
  *vucr++ = vec_adds (vbca, vucb);
  *vucr++ = vec_adds (vuca, vbcb);
  *vsir++ = vec_adds (vbia, vsib);
  *vsir++ = vec_adds (vsia, vbib);
  *vuir++ = vec_adds (vbia, vuib);
  *vuir++ = vec_adds (vuia, vbib);
  *vssr++ = vec_adds (vbsa, vssb);
  *vssr++ = vec_adds (vssa, vbsb);
  *vusr++ = vec_adds (vbsa, vusb);
  *vusr++ = vec_adds (vusa, vbsb);
  *vucr++ = vec_bperm (vuca, vucb);
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
  *vusr++ = vec_pmsum_be (vuca, vucb);
  *vuir++ = vec_pmsum_be (vusa, vusb);
  *vulr++ = vec_pmsum_be (vuia, vuib);
  *vuxr++ = vec_pmsum_be (vula, vulb);
  *vuir++ = vec_shasigma_be (vuia, 0, 1);
  *vulr++ = vec_shasigma_be (vula, 0, 1);
  *vsir++ = vec_subc (vsia, vsib);
  *vuir++ = vec_subc (vuia, vuib);
  *vsxr++ = vec_subc (vsxa, vsxb);
  *vuxr++ = vec_subc (vuxa, vuxb);
  *vsir++ = vec_sube (vsia, vsib, vsic);
  *vuir++ = vec_sube (vuia, vuib, vuic);
  *vsxr++ = vec_sube (vsxa, vsxb, vsxc);
  *vuxr++ = vec_sube (vuxa, vuxb, vuxc);
  *vsir++ = vec_subec (vsia, vsib, vsic);
  *vuir++ = vec_subec (vuia, vuib, vuic);
  *vsxr++ = vec_subec (vsxa, vsxb, vsxc);
  *vuxr++ = vec_subec (vuxa, vuxb, vuxc);
  *vscr++ = vec_subs (vsca, vscb);
  *vucr++ = vec_subs (vuca, vucb);
  *vsir++ = vec_subs (vsia, vsib);
  *vuir++ = vec_subs (vuia, vuib);
  *vssr++ = vec_subs (vssa, vssb);
  *vusr++ = vec_subs (vusa, vusb);
  *vsir++ = vec_sum2s (vsia, vsib);
  *vsir++ = vec_sum4s (vsca, vsib);
  *vsir++ = vec_sum4s (vssa, vsib);
  *vuir++ = vec_sum4s (vuca, vuib);

}

/* { dg-final { scan-assembler-times "vaddcuq" 2 } } */
/* { dg-final { scan-assembler-times "vaddeuqm" 2 } } */
/* { dg-final { scan-assembler-times "vaddecuq" 2 } } */
/* { dg-final { scan-assembler-times "vaddcuw" 6 } } */
/* { dg-final { scan-assembler-times "vadduwm" 4 } } */
/* { dg-final { scan-assembler-times "vsubcuq" 2 } } */
/* { dg-final { scan-assembler-times "vsubeuqm" 2 } } */
/* { dg-final { scan-assembler-times "vsubecuq" 2 } } */
/* { dg-final { scan-assembler-times "vsubcuw" 4 } } */
/* { dg-final { scan-assembler-times "vsubuwm" 4 } } */
/* { dg-final { scan-assembler-times "vbpermq" 2 } } */
/* { dg-final { scan-assembler-times "xxleqv" 4 } } */
/* { dg-final { scan-assembler-times "vgbbd" 1 } } */
/* { dg-final { scan-assembler-times "xxlnand" 4 } } */
/* { dg-final { scan-assembler-times "xxlorc" 4 } } */
/* { dg-final { scan-assembler-times "vperm" 1 } } */
/* { dg-final { scan-assembler-times "vpmsumb" 1 } } */
/* { dg-final { scan-assembler-times "vpmsumh" 1 } } */
/* { dg-final { scan-assembler-times "vpmsumw" 1 } } */
/* { dg-final { scan-assembler-times "vpmsumd" 1 } } */
/* { dg-final { scan-assembler-times "vshasigmaw" 1 } } */
/* { dg-final { scan-assembler-times "vshasigmad" 1 } } */
/* { dg-final { scan-assembler-times "vsubsbs" 1 } }   vec_subs */
/* { dg-final { scan-assembler-times "vsububs" 1 } }   vec_subs */
/* { dg-final { scan-assembler-times "vsubsws" 1 } }   vec_subs */
/* { dg-final { scan-assembler-times "vsubuws" 1 } }   vec_subs */
/* { dg-final { scan-assembler-times "vsubshs" 1 } }   vec_subs */
/* { dg-final { scan-assembler-times "vsubuhs" 1 } }   vec_subs */
/* { dg-final { scan-assembler-times "vsum2sws" 1 } }  vec_sum2s */
/* { dg-final { scan-assembler-times "vsum4sws" 0 } }  vec_sum4s */
/* { dg-final { scan-assembler-times "vsum4shs" 1 } }  vec_sum4s */
/* { dg-final { scan-assembler-times "vsum4ubs" 1 } }  vec_sum4s */

