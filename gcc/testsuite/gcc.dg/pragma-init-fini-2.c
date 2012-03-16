/* Tests for #pragma init and #pragma fini.  */

/* { dg-do link { target *-*-solaris2.* } } */
/* { dg-options "-fpic" } */

#include <stdio.h>

#pragma fini (f)

void f() {
  fprintf (stderr, "f\n");
}

int main () {
}
