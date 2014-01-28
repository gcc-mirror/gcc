/*
   { dg-options "-D _GNU_SOURCE -fdiagnostics-show-caret" }
   { dg-do compile }
 */

#line 4636 "configure"
#include <xxxxxxxxxxxx.h>
int main() { return 0; }

/* { dg-error "No such file or directory" { target *-*-* } 4636 } */
