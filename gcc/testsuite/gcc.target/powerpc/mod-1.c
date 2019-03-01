/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

int ismod (int a, int b) { return a%b; }
long lsmod (long a, long b) { return a%b; }
unsigned int iumod (unsigned int a, unsigned int b) { return a%b; }
unsigned long lumod (unsigned long a, unsigned long b) { return a%b; }

/* { dg-final { scan-assembler-times "modsw " 1 } } */
/* { dg-final { scan-assembler-times "modsd " 1 } } */
/* { dg-final { scan-assembler-times "moduw " 1 } } */
/* { dg-final { scan-assembler-times "modud " 1 } } */
/* { dg-final { scan-assembler-not   "mullw "   } } */
/* { dg-final { scan-assembler-not   "mulld "   } } */
/* { dg-final { scan-assembler-not   "divw "    } } */
/* { dg-final { scan-assembler-not   "divd "    } } */
/* { dg-final { scan-assembler-not   "divwu "   } } */
/* { dg-final { scan-assembler-not   "divdu "   } } */
