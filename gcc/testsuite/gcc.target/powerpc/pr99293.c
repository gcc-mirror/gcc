/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* Test for PR 99263, which wants to do:
	__builtin_vec_splats (__builtin_vec_extract (v, n))

   where v is a V2DF or V2DI vector and n is either 0 or 1.  Previously the
   compiler would do a direct move to the GPR registers to select the item and a
   direct move from the GPR registers to do the splat.  */

vector long long splat_dup_l_0 (vector long long v)
{
  return __builtin_vec_splats (__builtin_vec_extract (v, 0));
}

vector long long splat_dup_l_1 (vector long long v)
{
  return __builtin_vec_splats (__builtin_vec_extract (v, 1));
}

/* { dg-final { scan-assembler-times "xxpermdi" 2 } } */
