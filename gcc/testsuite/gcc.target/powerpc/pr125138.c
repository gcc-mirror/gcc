/* PR target/125138 */
/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O3 -mdejagnu-cpu=power10" } */

#include <altivec.h>

typedef vector unsigned char vui8_t;

vui8_t
test1 (vui8_t vra0, vui8_t vrb0, vui8_t vra1, vui8_t vrb1, vui8_t vrc)
{
  vui8_t perm0, perm1;
  perm0 = vec_permx (vra0, vrb0, vrc, 0);
  perm1 = vec_permx (vra1, vrb1, vrc, 1);
  return vec_or (perm0, perm1);
}

vui8_t
test2 (vui8_t vra0, vui8_t vrb0, vui8_t vra1, vui8_t vrb1, vui8_t vra2,
       vui8_t vrb2, vui8_t vra3, vui8_t vrb3, vui8_t vrc)
{
  vui8_t perm0, perm1, perm2, perm3;
  perm0 = vec_permx (vra0, vrb0, vrc, 0);
  perm1 = vec_permx (vra1, vrb1, vrc, 1);
  perm2 = vec_permx (vra2, vrb2, vrc, 2);
  perm3 = vec_permx (vra3, vrb3, vrc, 3);
  perm0 = vec_or (perm0, perm1);
  perm2 = vec_or (perm2, perm3);
  return vec_or (perm0, perm2);
}

/* { dg-final { scan-assembler-times {\mxxlnor\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxpermx\M} 6 } } */
