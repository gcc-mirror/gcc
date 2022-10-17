/* PR target/106550 */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -fdisable-rtl-split1" } */
/* force the constant splitter run after RA: -fdisable-rtl-split1.  */

void
foo (unsigned long long *a)
{
  /* Test oris/ori is used where paddi does not work with 'r0'. */
  register long long d asm("r0") = 0x1245abcef9240dec; /* pli+sldi+oris+ori */
  long long n;
  asm("cntlzd %0, %1" : "=r"(n) : "r"(d));
  *a++ = n;

  *a++ = 0x235a8470a7480000ULL; /* pli+sldi+oris */
  *a++ = 0x23a184700000b677ULL; /* pli+sldi+ori */
}

/* { dg-final { scan-assembler-times {\mpli\M} 3 } } */
/* { dg-final { scan-assembler-times {\msldi\M} 3 } } */
/* { dg-final { scan-assembler-times {\moris\M} 2 } } */
/* { dg-final { scan-assembler-times {\mori\M} 2 } } */
