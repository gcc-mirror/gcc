/*
  { dg-options "-Wuninitialized -ftrack-macro-expansion=2" }
  { dg-do compile }
*/

void f (unsigned);

#define CODE_WITH_WARNING \
  int a; /* { dg-message "was declared here" } */	 \
  f (a)	 /* { dg-error "used uninitialized" } */

#pragma GCC diagnostic ignored "-Wuninitialized"

void
g (void)
{
  /* No warning expected here since the #pragma is in effect.  */
  CODE_WITH_WARNING;
}

#pragma GCC diagnostic error "-Wuninitialized"

void
h (void)
{
  CODE_WITH_WARNING; /* { dg-message "in expansion of macro 'CODE_WITH_WARNING'" } */
}

/* { dg-regexp {.*some warnings being treated as errors} } */
