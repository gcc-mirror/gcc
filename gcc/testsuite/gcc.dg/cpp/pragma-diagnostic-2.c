/*
  { dg-options "-Wuninitialized -ftrack-macro-expansion=2" }
  { dg-do compile }
*/

void f (unsigned);

#define CODE_WITH_WARNING \
  int a; /* { dg-message "expansion|declared here" } */  \
  f (a)	 /* { dg-message "expansion" } */

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
  CODE_WITH_WARNING;		/* { dg-message "expanded" } */
}

/*
  { dg-message "some warnings being treated as errors" "" {target *-*-*} 0 }
*/

/* { dg-error "uninitialized" "" { target *-*-* } { 10 } } */
