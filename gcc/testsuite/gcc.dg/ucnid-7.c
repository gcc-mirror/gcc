/* Verify diagnostics for extended identifiers refer to UCNs (in the C
   locale).  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -fextended-identifiers" } */
/* { dg-require-ascii-locale "" } */

void *p = &\u00e9; /* { dg-error "'\\\\U000000e9' undeclared" } */
void *q = &\u1e00; /* { dg-error "'\\\\U00001e00' undeclared" } */
