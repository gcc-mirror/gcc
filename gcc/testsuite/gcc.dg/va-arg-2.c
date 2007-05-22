/* <varargs.h> is not supported anymore, but we do install a stub
   file which issues an #error telling the user to convert their code.  */

/* { dg-do compile } */

#include <varargs.h>  /* { dg-bogus "varargs.h" "missing file" } */

/* { dg-message "" "In file included from" { target *-*-* } 6 } */
/* { dg-error "no longer implements" "#error 1" { target *-*-* } 4 } */
/* { dg-error "Revise your code" "#error 2" { target *-*-* } 5 } */

int x;  /* prevent empty-source-file warning */
