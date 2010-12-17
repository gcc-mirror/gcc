/* Tests for #pragma init and #pragma fini.  */

/* { dg-do link { target *-*-solaris2.* } } */
/* { dg-options "-fpic" } */
/* { dg-xfail-if "no .pushsection/.popsection" { i?86-*-solaris2.8 && { ! gas } } } */

#include <stdio.h>

#pragma fini (f)

void f() {
  fprintf (stderr, "f\n");
}

int main () {
}
