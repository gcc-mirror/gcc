/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -mno-power8-vector -O2" } */

#include <altivec.h>

/* Test VSX built-ins added for version 1.1 of ELFv2 ABI.  */

vector bool     long long vbla, vblb, vblc;
vector bool char vbca, vbcb, vbcc;
vector bool int vbia, vbib, vbic;
vector signed char vsca, vscb, vscc;
vector unsigned char vuca, vucb, vucc;
vector signed int vsia, vsib, vsic;
vector unsigned int vuia, vuib, vuic;

vector unsigned long long vulla, vullb, vullc;
vector signed long long vslla, vsllb, vsllc;
vector bool long long vblla, vbllb, vbllc;
vector bool short int vbsia, vbsib, vbsic;
vector signed short int vssia, vssib, vssic;
vector unsigned short int vusia, vusib, vusic;
vector double vda, vdb;
vector float vfa, vfb;

void foo (vector bool long long *vblr,
	  vector double *vdr, vector unsigned long long *vullz,
	  vector double *vdz, vector bool char *vbcz,
	  vector signed char *vscz, vector unsigned char *vucz,
	  vector bool int *vbiz, vector int *viz,
	  vector unsigned int *vuiz, vector signed long long int *vslliz,
	  vector bool short int *vbsiz, vector signed short int *vssiz,
	  vector unsigned short int *vusiz, vector float *vfz)
{
  *vblr++ = vec_andc (vbla, vblb);
  *vdr++  = vec_double (vslla);
  *vdr++  = vec_double (vulla);

  *vblr++ = vec_mergeh (vbla, vblb);
  *vblr++ = vec_mergel (vbla, vblb);
  *vblr++ = vec_nor (vbla, vblb);
  *vblr++ = vec_or (vbla, vblb);
  *vblr++ = vec_sel (vbla, vblb, vblc);
  *vblr++ = vec_sel (vbla, vblb, vullc);
  *vblr++ = vec_xor (vbla, vblb);

  *vullz++ = vec_sel (vulla, vullb, vbllc);
  *vullz++ = vec_sel (vulla, vullb, vullc);

  *vdz++ = vec_sel(vda, vdb, vullc);

  *vbcz++ = vec_sel (vbca, vbcb, vbcc);
  *vbcz++ = vec_sel (vbca, vbcb, vucc);
  *vbcz++ = vec_xor (vbca, vbcb);
  *vscz++ = vec_sel (vsca, vscb, vbcc);
  *vscz++ = vec_sel (vsca, vscb, vucc);
  *vucz++ = vec_sel (vuca, vucb, vbcc);
  *vucz++ = vec_sel (vuca, vucb, vucc);

  *vbiz++ = vec_sel (vbia, vbib, vbic);
  *vbiz++ = vec_sel (vbia, vbib, vuic);
  *vbiz++ = vec_xor (vbia, vbib);
  *viz++ = vec_sel (vsia, vsib, vbic);
  *viz++ = vec_sel (vsia, vsib, vuic);
  *vuiz++ = vec_sel (vuia, vuib, vbic);
  *vuiz++ = vec_sel (vuia, vuib, vuic);

  *vslliz++ = vec_sel(vslla, vsllb, vbllc);
  *vslliz++ = vec_sel(vslla, vsllb, vullc);

  *vssiz++ = vec_sel(vssia, vssib, vbsic);
  *vssiz++ = vec_sel(vssia, vssib, vusic);
  *vusiz++ = vec_sel(vusia, vusib, vbsic);
  *vusiz++ = vec_sel(vusia, vusib, vusic);

  *vbsiz++ = vec_sel (vbsia, vbsib, vbsic);
  *vbsiz++ = vec_sel (vbsia, vbsib, vusic);
  *vbsiz++ = vec_xor (vbsia, vbsib);

  *vdz++ = vec_sel (vda, vdb, vbllc);
  *vfz++ = vec_sel (vfa, vfb, vbic);
  *vfz++ = vec_sel (vfa, vfb, vuic);
  *vfz++ = vec_xor (vfa, vfb);
}

/* { dg-final { scan-assembler-times "xxlandc" 1 } } */
/* { dg-final { scan-assembler-times "xvcvsxddp" 1 } } */
/* { dg-final { scan-assembler-times "xvcvuxddp" 1 } } */
/* { dg-final { scan-assembler-times "xxpermdi .*,.*,.*,3" 1 } } */
/* { dg-final { scan-assembler-times "xxpermdi .*,.*,.*,0" 1 } } */
/* { dg-final { scan-assembler-times "xxlnor" 1 } } */
/* { dg-final { scan-assembler-times "xxlor" 1 } } */
/* { dg-final { scan-assembler-times "xxsel" 28 } } */
/* { dg-final { scan-assembler-times "xxlxor" 5 } } */
