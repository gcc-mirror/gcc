/* { dg-do preprocess } */
/* { dg-options "-CC -traditional-cpp" } */

/* This tests to make sure that comments are ignored between # and the
   directive name when the -CC option is used.

   Jason R. Thorpe, 6 Apr 2002  */

#/**/define def passed

def

/*
   { dg-final { if ![file exists maccom1.i] { return }                    } }
   { dg-final { if { [grep maccom1.i "^passed"] != "" } { return }        } }
   { dg-final { fail "maccom1.c: comment between # and directive name with -CC" } }
*/
