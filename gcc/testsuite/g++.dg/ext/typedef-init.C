/* { dg-do compile } */
/* { dg-options "-fpermissive" } // suppress default -pedantic-errors */

/* This code used to be a legitimate, if dubious, extension.  However,
   it's been broken since GCC 3.0 (caused ICE) and we have now removed
   the extension.  See PR c/7353.

   C++ issues a warning in addition to the error, since this construct
   appears to be a case of implicit int (forbidden in std. C++) until
   we get to the equals sign.  */

typedef A = 0;  /* { dg-error "initialized" "typedef A = B" } */
                /* { dg-warning "no type" "also warns" { target *-*-* } 12 } */
A a;            /* { dg-bogus "" "no error cascade" } */
