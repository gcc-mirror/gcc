/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test that PLI (PADDI) is generated to load a large constant for SImode.  */
void
large_si (unsigned int *p)
{
  *p = 0x12345U;
}

/* { dg-final { scan-assembler {\mpli\M} } } */
