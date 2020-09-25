/* PR testsuite/52665
 * Make sure scan-assembler turns off .ident unless -fident in testcase */
/* { dg-do compile } */
/* { dg-options "-fident" } */
/* { dg-require-effective-target ident_directive }*/
int i;

/* { dg-final { scan-assembler "GCC: " { xfail { { hppa*-*-hpux* && { ! lp64 } } || { powerpc-ibm-aix* || powerpc*-*-darwin* } } } } } */
