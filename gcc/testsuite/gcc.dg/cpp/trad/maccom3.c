/* { dg-do preprocess } */
/* { dg-options "-CC -traditional-cpp" } */

/* This tests to make sure that comments in the definition of a macro
   parameter list are ignored when the -CC option is used.

   Jason R. Thorpe, 6 Apr 2002  */

#define def(x /**/, y) passed

def(x,y)

/*
   { dg-final { if ![file exists maccom3.i] { return }                     } }
   { dg-final { if { [grep maccom3.i "^passed"] != "" } { return }         } }
   { dg-final { fail "maccom3.c: comment in macro parameter list with -CC" } }
*/
