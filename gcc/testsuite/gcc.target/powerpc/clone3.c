/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

/* Power9 (aka, ISA 3.0) has a MODSD instruction to do modulus, while Power8
   (aka, ISA 2.07) has to do modulus with divide and multiply.  Make sure
   both clone functions are generated.

   FUTURE has pc-relative instructions to access static values, while earlier
   systems used TOC addressing.

   Restrict ourselves to Linux, since IFUNC might not be supported in other
   operating systems.  */

static long s;
long *p = &s;

__attribute__((target_clones("cpu=power10,cpu=power9,default")))
long mod_func (long a, long b)
{
  return (a % b) + s;
}

long mod_func_or (long a, long b, long c)
{
  return mod_func (a, b) | c;
}

/* { dg-final { scan-assembler-times {\mdivd\M}  1 } } */
/* { dg-final { scan-assembler-times {\mmulld\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmodsd\M} 2 } } */
/* { dg-final { scan-assembler-times {\mpld\M}   1 } } */
