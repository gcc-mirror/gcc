/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

long
s_madd (long a, long b, long c)
{
  return (a * b) + c;
}

unsigned long
u_madd (unsigned long a, unsigned long b, unsigned long c)
{
  return (a * b) + c;
}

/* { dg-final { scan-assembler-times "maddld " 2 } } */
/* { dg-final { scan-assembler-not   "mulld "    } } */
/* { dg-final { scan-assembler-not   "add "      } } */
