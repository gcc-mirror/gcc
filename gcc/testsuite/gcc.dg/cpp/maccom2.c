/* { dg-do preprocess } */
/* { dg-options "-CC" } */

/* This tests to make sure that comments between the #define directive
   and the macro identifier are ignored (i.e. treated like whitespace)
   when the -CC option is used.

   Jason R. Thorpe, 6 Apr 2002  */

#define/**/def passed

def

/*
   { dg-final { if ![file exists maccom2.i] { return }                    } }
   { dg-final { if { [grep maccom2.i "^passed"] != "" } { return }        } }
   { dg-final { fail "maccom2.c: comment between #define and identifier with -CC" } }
*/
