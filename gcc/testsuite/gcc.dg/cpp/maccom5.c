/* { dg-do preprocess } */
/* { dg-options "-CC" } */

/* This tests to make sure that C++ comments are converted to C comments
   when saved in the macro and copied to the output file when the macro
   is expanded when the -CC option is used.

   Jason R. Thorpe, 6 Apr 2002  */

#define def // passed

def:

/*
   /* The + in the regexp prevents it from matching itself.  */
   { dg-final { if ![file exists maccom5.i] { return }                    } }
   { dg-final { if \{ [grep maccom5.i "p+assed"] != "" \} \{              } }
   { dg-final { if \{ [grep maccom5.i "p+assed:"] == "" \} \{             } }
   { dg-final { return \} \}                                              } }
   { dg-final { fail "maccom5.c: C++ comment in macro expansion with -CC" } }
*/
