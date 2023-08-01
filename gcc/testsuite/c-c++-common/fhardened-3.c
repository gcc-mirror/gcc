/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -O0" } */
/* Test that we don't get any diagnostic coming from libc headers.  */

#include <stdio.h>

/* The most useful C program known to man.  */

int
main ()
{
}

/* { dg-warning "._FORTIFY_SOURCE. is not enabled" "" { target *-*-* } 0 } */
