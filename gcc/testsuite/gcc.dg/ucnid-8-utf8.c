/* Verify diagnostics for extended identifiers refer to UCNs (in the C
   locale).  Further tests of C front-end diagnostics.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wvla" } */
/* { dg-require-ascii-locale "" } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */

int a __attribute__((__mode__(é))); /* { dg-error "unknown machine mode '\\\\U000000e9'" } */
struct s1 { int é : 0; }; /* { dg-error "zero width for bit-field '\\\\U000000e9'" } */

void f (int b) { int é[b]; } /* { dg-warning "variable length array '\\\\U000000e9'" } */

void g (static int é); /* { dg-error "storage class specified for parameter '\\\\U000000e9'" } */

struct s2 { int á; } é = { { 0 } }; /* { dg-warning "braces around scalar initializer" } */
/* { dg-message "near initialization for '\\\\U000000e9\\.\\\\U000000e1'" "UCN diag" { target *-*-* } .-1 } */
