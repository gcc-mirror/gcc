/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

static int mem;
int *ptr = &mem;

long
add (long *p, int reg)
{
  __asm__ (" #foo %0" : "+r" (reg));
  return p[reg] + p[mem];
}

/* { dg-final { scan-assembler-times "extswsli " 2 } } */
/* { dg-final { scan-assembler-times "lwz "      1 } } */
/* { dg-final { scan-assembler-not   "lwa "        } } */
/* { dg-final { scan-assembler-not   "sldi "       } } */
/* { dg-final { scan-assembler-not   "extsw "      } } */
