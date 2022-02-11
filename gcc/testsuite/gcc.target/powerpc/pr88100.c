/* PR88100.  Verify that rs6000 gimple-folding code handles the
   vec_splat_{su}{8,16,32} invalid data properly. */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

vector unsigned char
splatu1 (void)
{
  return vec_splat_u8(0x100);/* { dg-error "argument 1 must be a literal between -16 and 15, inclusive" } */
}

vector unsigned short
splatu2 (void)
{
  return vec_splat_u16(0x10000);/* { dg-error "argument 1 must be a literal between -16 and 15, inclusive" } */
}

vector unsigned int
splatu3 (void)
{
  return vec_splat_u32(0x10000000);/* { dg-error "argument 1 must be a literal between -16 and 15, inclusive" } */
}

vector signed char
splats1 (void)
{
  return vec_splat_s8(0x100);/* { dg-error "argument 1 must be a literal between -16 and 15, inclusive" } */
}

vector signed short
splats2 (void)
{
  return vec_splat_s16(0x10000);/* { dg-error "argument 1 must be a literal between -16 and 15, inclusive" } */
}

vector signed int
splats3 (void)
{
  return vec_splat_s32(0x10000000);/* { dg-error "argument 1 must be a literal between -16 and 15, inclusive" } */
}
