/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -mno-power8-vector -O2" } */

#include <altivec.h>

/* Test VSX built-ins added for version 1.1 of ELFv2 ABI.  */

vector bool     long long vbla, vblb, vblc;
vector signed   long long vsla;
vector unsigned long long vula, vulc;

void foo (vector bool long long *vblr,
	  vector double *vdr)
{
  *vblr++ = vec_andc (vbla, vblb);
  *vdr++  = vec_double (vsla);
  *vdr++  = vec_double (vula);
  *vblr++ = vec_mergeh (vbla, vblb);
  *vblr++ = vec_mergel (vbla, vblb);
  *vblr++ = vec_nor (vbla, vblb);
  *vblr++ = vec_or (vbla, vblb);
  *vblr++ = vec_sel (vbla, vblb, vblc);
  *vblr++ = vec_sel (vbla, vblb, vulc);
  *vblr++ = vec_xor (vbla, vblb);
}

/* { dg-final { scan-assembler-times "xxlandc" 1 } } */
/* { dg-final { scan-assembler-times "xvcvsxddp" 1 } } */
/* { dg-final { scan-assembler-times "xvcvuxddp" 1 } } */
/* { dg-final { scan-assembler-times "xxpermdi .*,.*,.*,3" 1 } } */
/* { dg-final { scan-assembler-times "xxpermdi .*,.*,.*,0" 1 } } */
/* { dg-final { scan-assembler-times "xxlnor" 1 } } */
/* { dg-final { scan-assembler-times "xxlor" 1 } } */
/* { dg-final { scan-assembler-times "xxsel" 2 } } */
/* { dg-final { scan-assembler-times "xxlxor" 1 } } */
