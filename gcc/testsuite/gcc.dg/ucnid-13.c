/* Verify diagnostics for extended identifiers refer to UCNs (in the C
   locale).  Miscellaneous diagnostics.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fextended-identifiers -Wpacked" } */
/* { dg-require-ascii-locale "" } */

int a __attribute__((\u00c0)); /* { dg-warning "'\\\\U000000c0' attribute directive ignored" } */

extern void \u00c1 (void) __attribute__((deprecated));
void g (void) { \u00c1 (); } /* { dg-warning "'\\\\U000000c1' is deprecated" } */

struct \u00c2 { char c; } __attribute__((packed)); /* { dg-warning "'\\\\U000000c2'" } */

void h (void) { asm ("%[\u00c3]" : : ); } /* { dg-error "undefined named operand '\\\\U000000c3'" } */
