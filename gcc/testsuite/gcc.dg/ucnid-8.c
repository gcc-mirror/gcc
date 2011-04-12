/* Verify diagnostics for extended identifiers refer to UCNs (in the C
   locale).  Further tests of C front-end diagnostics.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fextended-identifiers -Wvla" } */
/* { dg-require-ascii-locale "" } */

int a __attribute__((__mode__(\u00e9))); /* { dg-error "unknown machine mode '\\\\U000000e9'" } */
struct s1 { int \u00e9 : 0; }; /* { dg-error "zero width for bit-field '\\\\U000000e9'" } */

void f (int b) { int \u00e9[b]; } /* { dg-warning "variable length array '\\\\U000000e9'" } */

void g (static int \u00e9); /* { dg-error "storage class specified for parameter '\\\\U000000e9'" } */

struct s2 { int \u00e1; } \u00e9 = { { 0 } }; /* { dg-warning "braces around scalar initializer" } */
/* { dg-warning "near initialization for '\\\\U000000e9\\.\\\\U000000e1'" "UCN diag" { target *-*-* } 14 } */
