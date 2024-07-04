/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#ifndef TYPE
#define TYPE vector double
#endif

struct foo {
  TYPE a, b, c, d;
};

/* Test whether ISA 3.0 vector d-form instructions are implemented.  */
void
add (struct foo *p)
{
  p->b = p->c + p->d;
}

/* Make sure we don't use direct moves to get stuff into GPR registers.  */
void
gpr (struct foo *p)
{
  TYPE x = p->c;

  __asm__ (" # reg = %0" : "+r" (x));

  p->b = x;
}

/* { dg-final { scan-assembler     "lxv "      } } */
/* { dg-final { scan-assembler     "stxv "     } } */
/* { dg-final { scan-assembler-not "lxvx "     } } */
/* { dg-final { scan-assembler-not "stxvx "    } } */
/* { dg-final { scan-assembler-not "mfvsrd "   } } */
/* { dg-final { scan-assembler-not "mfvsrld "  } } */
/* { dg-final { scan-assembler     "l\[dq\] "  } } */
/* { dg-final { scan-assembler     "st\[dq\] " } } */
