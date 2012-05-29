/*
  { dg-options "-Wuninitialized -ftrack-macro-expansion=2" }
  { dg-do compile }
*/

void f (unsigned);

#define CODE_WITH_WARNING \
  int a; /* { dg-message "was declared here" } */	 \
  f (a)	 /* { dg-warning "used uninitialized" } */

#pragma GCC diagnostic ignored "-Wuninitialized"

void
g (void)
{
  CODE_WITH_WARNING;
}

#pragma GCC diagnostic push

#pragma GCC diagnostic error "-Wuninitialized"

void
h (void)
{
  CODE_WITH_WARNING; /* { dg-message "in expansion of macro 'CODE_WITH_WARNING'" } */
}
