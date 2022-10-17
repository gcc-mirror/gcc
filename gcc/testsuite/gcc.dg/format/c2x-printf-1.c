/* Test for printf formats.  Formats using C2X features.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic -Wformat" } */

#include "format.h"

void
foo (unsigned int u, unsigned short us, unsigned char uc, unsigned long ul,
     unsigned long long ull, uintmax_t uj, size_t z, unsigned_ptrdiff_t ut)
{
  /* Use of %b with each length modifier and other valid features.  */
  printf ("%b %hb %hhb %lb %llb %jb %zb %tb\n", u, us, uc, ul, ull, uj, z, ut);
  printf ("%*.*llb\n", 1, 2, ull);
  printf ("%-b\n", u);
  printf ("%#b\n", u);
  printf ("%08b\n", u);
  /* Flags valid on signed conversions only.  */
  printf ("%+b\n", u); /* { dg-warning "flag" } */
  printf ("% b\n", u); /* { dg-warning "flag" } */
  /* Flags ignored in certain combinations.  */
  printf ("%-08b\n", u); /* { dg-warning "ignored" } */
  printf ("%08.5b\n", u); /* { dg-warning "ignored" } */
  /* Use of 'L' and 'q' for long long is an extension.  */
  printf ("%Lb", ull); /* { dg-warning "does not support" } */
  printf ("%qb", ull); /* { dg-warning "does not support" } */
}
