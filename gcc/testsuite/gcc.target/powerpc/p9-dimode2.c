/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Verify that large integer constants are loaded via direct move instead of
   being loaded from memory.  */

double
p9_large (void)
{
  long l = 0x12345678;
  double ret;

  __asm__ ("xxlor %x0,%x1,%x1" : "=&d" (ret) : "wa" (l));

  return ret;
}

/* { dg-final { scan-assembler     {\mmtvsr}    } } */
/* { dg-final { scan-assembler-not {\mld\M}     } } */
/* { dg-final { scan-assembler-not {\mlfd\M}    } } */
/* { dg-final { scan-assembler-not {\mlxsd\M}   } } */
