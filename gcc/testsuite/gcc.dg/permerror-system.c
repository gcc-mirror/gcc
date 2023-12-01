/* { dg-options "-isystem ${srcdir}" } */

/* Test that permerrors appear in system headers.  */

/* The dg-* directives in the header file are ignored.  No warnings are
   expected.  */
#include <gcc.dg/permerror-default.c>

/* These errors come from permerror-default.c.  No errors yet.  */
