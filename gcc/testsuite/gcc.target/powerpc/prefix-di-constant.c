/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test that PLI (PADDI) is generated to load a large constant.  */
unsigned long long
large (void)
{
  return 0x12345678ULL;
}

/* { dg-final { scan-assembler {\mpli\M} } } */
