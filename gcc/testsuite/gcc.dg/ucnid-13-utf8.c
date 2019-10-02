/* Verify diagnostics for extended identifiers refer to UCNs (in the C
   locale).  Miscellaneous diagnostics.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wpacked" } */
/* { dg-require-ascii-locale "" } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */

int a __attribute__((À)); /* { dg-warning "'\\\\U000000c0' attribute directive ignored" } */

extern void Á (void) __attribute__((deprecated));
void g (void) { Á (); } /* { dg-warning "'\\\\U000000c1' is deprecated" } */

struct Â { char c; } __attribute__((packed)); /* { dg-warning "'\\\\U000000c2'" } */

void h (void) { asm ("%[Ã]" : : ); } /* { dg-error "undefined named operand '\\\\U000000c3'" } */
