/* Test for bugs with %z and %Z formats.  See PRs c/89, c/156, c/376.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include "format.h"

size_t
foo (void)
{
  size_t t;
  scanf ("%zu", &t); /* { dg-bogus "length|format" "bogus scanf warning" } */
  return t;
}

void
bar (size_t t)
{
  printf ("%zu\n", t); /* { dg-bogus "format" "bogus printf warning" } */
}

/* The %Z printf format is an old GNU libc extension to do the same thing.  */

void
baz (size_t t)
{
  printf ("%Zu\n", t); /* { dg-bogus "format" "bogus printf warning" } */
}
