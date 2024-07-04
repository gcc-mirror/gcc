/* { dg-additional-options "-include pr115312.H -save-temps" } */
#error "suppress PCH assembly comparison, which does not work with -save-temps" /* { dg-error "." } */
