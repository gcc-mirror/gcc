/* Expected instruction selection as characterized by
   scan-assembler-times directives below is only relevant to
   little-endian targets.  */
/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -mabi=altivec" } */
/* { dg-final { scan-assembler-times "lvx" 1 } } */
/* { dg-final { scan-assembler-times "stvx" 1 } } */
/* { dg-final { scan-assembler-not "ld" } } */
/* { dg-final { scan-assembler-not "lwz" } } */
/* { dg-final { scan-assembler-not "stw" } } */
/* { dg-final { scan-assembler-not "addi" } } */

typedef vector long long v2di_type;

v2di_type
return_v2di (v2di_type *ptr)
{
  /* As of pr48857, should generate lxvd2x 34,0,3
     followed by xxpermdi 34,34,34,2.  Subsequent optimization
     recognizes that ptr refers to an aligned vector and replaces
     this with lvx 2,0,3.  */
  return *ptr;
}

void
pass_v2di (v2di_type arg, v2di_type *ptr)
{
  /* As of pr48857, should generate xxpermdi 34,34,34,2 followed by
     stxvd2x 34,0,5.  Subsequent optimization recognizes that ptr
     refers to an aligned vector and replaces this with stvx 2,0,5.  */
  *ptr = arg;
}

