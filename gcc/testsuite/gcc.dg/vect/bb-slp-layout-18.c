/* { dg-do compile } */
/* { dg-require-effective-target vect_float} */
/* { dg-additional-options "-w -Wno-psabi -ffast-math" } */

typedef float v4sf __attribute__((vector_size(sizeof(float)*4)));

float __attribute__((noipa))
f(v4sf v0, v4sf v1)
{
  return v0[0]*v1[0]+v0[1]*v1[1]+v0[2]*v1[2]+v0[3]*v1[3];
}

/* We are lacking an effective target for .REDUC_PLUS support.  */
/* { dg-final { scan-tree-dump-times "basic block part vectorized" 1 "slp2" { target x86_64-*-* aarch64*-*-* } } } */
/* { dg-final { scan-tree-dump-not " = VEC_PERM_EXPR" "slp2" { target x86_64-*-* aarch64*-*-* } } } */
