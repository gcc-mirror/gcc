/* { dg-do compile } */
/* { dg-options "-Walways-true -Wattributes -Werror" } */
/* { dg-warning "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* This is the first in a series of test cases that test the
   interaction between -Wfoo, -Werror, -Werror=foo, and #pragma GCC
   diagnostic error foo.  This one has all the bits we're testing, the
   others are subsets of this one.  */

#pragma GCC diagnostic error "-Walways-true"

void __attribute__((dj)) bar() { }	/* { dg-warning "warning: .* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error "error: .* will always evaluate as 'true'" } */
    grill ();
}
