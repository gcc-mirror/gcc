/* Test for printf formats using string literal plus constant.
 */
/* Origin: Jakub Jelinek <jakub@redhat.com> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat=2" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int i)
{
  printf ("%%d\n" + 1, i);
  printf (5 + "%.-*d%3d\n", i);
  printf ("%d%d" + 2, i, i);	/* { dg-warning "arguments" "wrong number of args" } */
  printf (3 + "%d\n");		/* { dg-warning "zero-length" "zero-length string" } */
  printf ("%d\n" + i, i);	/* { dg-warning "not a string" "non-constant addend" } */
  printf ("%d\n" + 10);		/* { dg-warning "not a string" "too large addend" } */
  printf ("%d\n" - 1, i);	/* { dg-warning "not a string" "minus constant" } */
  printf ("%d\n" + -1, i);	/* { dg-warning "not a string" "negative addend" } */
}
