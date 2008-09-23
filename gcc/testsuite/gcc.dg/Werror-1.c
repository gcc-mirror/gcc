/* { dg-do compile } */
/* { dg-options "-Waddress -Wattributes -Werror -fshow-column" } */
/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* This is the first in a series of test cases that test the
   interaction between -Wfoo, -Werror, -Werror=foo, and #pragma GCC
   diagnostic error foo.  This one has all the bits we're testing, the
   others are subsets of this one.  */

#pragma GCC diagnostic error "-Waddress"

void __attribute__((dj)) bar() { }	/* { dg-error ".* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error "7:.* will always evaluate as 'true'" } */
    grill ();
}
