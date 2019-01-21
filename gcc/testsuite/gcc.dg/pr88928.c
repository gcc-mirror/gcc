/* { dg-do compile } */
/* { dg-options "-Wno-pedantic -Waddress-of-packed-member" } */
struct a { } __attribute__((__packed__));
void c (struct a **);
void d (const struct a *b) { c ((struct a **) b); }
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
