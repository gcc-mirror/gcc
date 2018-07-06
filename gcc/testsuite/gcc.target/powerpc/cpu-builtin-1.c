/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */

void
use_cpu_is_builtins (unsigned int *p)
{
  /* If GCC was configured to use an old GLIBC (before 2.23), the
     __builtin_cpu_is and __builtin_cpu_supports built-in functions return 0,
     and the compiler issues a warning that you need a newer glibc to use them.
     Use #ifdef to avoid the warning.  */
#ifdef __BUILTIN_CPU_SUPPORTS__
  p[0] = __builtin_cpu_is ("power9");
  p[1] = __builtin_cpu_is ("power8");
  p[2] = __builtin_cpu_is ("power7");
  p[3] = __builtin_cpu_is ("power6x");
  p[4] = __builtin_cpu_is ("power6");
  p[5] = __builtin_cpu_is ("power5+");
  p[6] = __builtin_cpu_is ("power5");
  p[7] = __builtin_cpu_is ("ppc970");
  p[8] = __builtin_cpu_is ("power4");
  p[9] = __builtin_cpu_is ("ppca2");
  p[10] = __builtin_cpu_is ("ppc476");
  p[11] = __builtin_cpu_is ("ppc464");
  p[12] = __builtin_cpu_is ("ppc440");
  p[13] = __builtin_cpu_is ("ppc405");
  p[14] = __builtin_cpu_is ("ppc-cell-be");
#else
  p[0] = 0;
#endif
}

void
use_cpu_supports_builtins (unsigned int *p)
{
#ifdef __BUILTIN_CPU_SUPPORTS__
  p[0] = __builtin_cpu_supports ("4xxmac");
  p[1] = __builtin_cpu_supports ("altivec");
  p[2] = __builtin_cpu_supports ("arch_2_05");
  p[3] = __builtin_cpu_supports ("arch_2_06");
  p[4] = __builtin_cpu_supports ("arch_2_07");
  p[5] = __builtin_cpu_supports ("arch_3_00");
  p[6] = __builtin_cpu_supports ("archpmu");
  p[7] = __builtin_cpu_supports ("booke");
  p[8] = __builtin_cpu_supports ("cellbe");
  p[9] = __builtin_cpu_supports ("dfp");
  p[10] = __builtin_cpu_supports ("dscr");
  p[11] = __builtin_cpu_supports ("ebb");
  p[12] = __builtin_cpu_supports ("efpdouble");
  p[13] = __builtin_cpu_supports ("efpsingle");
  p[14] = __builtin_cpu_supports ("fpu");
  p[15] = __builtin_cpu_supports ("htm");
  p[16] = __builtin_cpu_supports ("htm-nosc");
  p[17] = __builtin_cpu_supports ("ic_snoop");
  p[18] = __builtin_cpu_supports ("ieee128");
  p[19] = __builtin_cpu_supports ("isel");
  p[20] = __builtin_cpu_supports ("mmu");
  p[21] = __builtin_cpu_supports ("notb");
  p[22] = __builtin_cpu_supports ("pa6t");
  p[23] = __builtin_cpu_supports ("power4");
  p[24] = __builtin_cpu_supports ("power5");
  p[25] = __builtin_cpu_supports ("power5+");
  p[26] = __builtin_cpu_supports ("power6x");
  p[27] = __builtin_cpu_supports ("ppc32");
  p[28] = __builtin_cpu_supports ("ppc601");
  p[29] = __builtin_cpu_supports ("ppc64");
  p[30] = __builtin_cpu_supports ("ppcle");
  p[31] = __builtin_cpu_supports ("smt");
  p[32] = __builtin_cpu_supports ("spe");
  p[33] = __builtin_cpu_supports ("tar");
  p[34] = __builtin_cpu_supports ("true_le");
  p[35] = __builtin_cpu_supports ("ucache");
  p[36] = __builtin_cpu_supports ("vcrypto");
  p[37] = __builtin_cpu_supports ("vsx");
  p[38] = __builtin_cpu_supports ("darn");
  p[39] = __builtin_cpu_supports ("scv");
  p[40] = __builtin_cpu_supports ("htm-no-suspend");
#else
  p[0] = 0;
#endif
}
