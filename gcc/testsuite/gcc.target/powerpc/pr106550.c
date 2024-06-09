/* PR target/106550 */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */
/* { dg-require-effective-target has_arch_ppc64 } */

void
foo (unsigned long long *a)
{
  *a++ = 0x020805006106003; /* pli+pli+rldimi */
  *a++ = 0x2351847027482577;/* pli+pli+rldimi */  
}

/* { dg-final { scan-assembler-times {\mpli\M} 4 } } */
/* { dg-final { scan-assembler-times {\mrldimi\M} 2 } } */

