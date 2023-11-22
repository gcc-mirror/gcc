/* { dg-do compile { target { ! default_packed } } } */
/* { dg-options "-Wno-pedantic -Waddress-of-packed-member -Wcast-align=strict" } */
struct a { } __attribute__((__packed__));
void c (struct a **);
void d (const struct a *b) { c ((struct a **) b); }
/* { dg-warning "increases required alignment" "" { target *-*-* } .-1 } */
