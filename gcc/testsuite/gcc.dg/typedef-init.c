/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

/* This code used to be a legitimate, if dubious, extension.  However,
   it's been broken since GCC 3.0 (caused ICE) and we have now removed
   the extension.  See PR c/7353.  */

typedef A = 0;  /* { dg-error "initialized" "typedef A = B" } */
A a;            /* { dg-bogus "" "no error cascade" } */
