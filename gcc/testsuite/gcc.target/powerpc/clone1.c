/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

/* Power9 (aka, ISA 3.0) has a MODSD instruction to do modulus, while Power8
   (aka, ISA 2.07) has to do modulus with divide and multiply.  Make sure
   both clone functions are generated.

   Restrict ourselves to Linux, since IFUNC might not be supported in other
   operating systems.  */

__attribute__((target_clones("cpu=power9,default")))
long mod_func (long a, long b)
{
  return a % b;
}

long mod_func_or (long a, long b, long c)
{
  return mod_func (a, b) | c;
}

/* { Fail due to RS6000_DISABLE_SCALAR_MODULO. */
/* { dg-final { scan-assembler-times {\mdivd\M}  1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\mmulld\M} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\mmodsd\M} 1 { xfail *-*-* } } } */
