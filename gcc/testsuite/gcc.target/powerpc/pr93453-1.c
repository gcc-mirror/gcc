/* { dg-do compile { target has_arch_ppc64 } } */
/* { dg-options "-mdejagnu-cpu=power6 -O2" } */

unsigned long load_byte_reverse (unsigned long *in)
{
   return __builtin_bswap64 (*in);
}

unsigned long byte_reverse (unsigned long in)
{
   return __builtin_bswap64 (in);
}

/* { dg-final { scan-assembler-times {\mrldimi\M} 2 } } */
