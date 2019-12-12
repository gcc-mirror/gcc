/* Verify diagnostics for extended identifiers refer to UCNs (in the C
   locale).  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */
/* { dg-require-ascii-locale "" } */
/* { dg-skip-if "" { "powerpc-ibm-aix*" } } */

void *p = &é; /* { dg-error "'\\\\U000000e9' undeclared" } */
void *q = &Ḁ; /* { dg-error "'\\\\U00001e00' undeclared" } */
