/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

/* This code used to be a legitimate, if dubious, extension.  However,
   it's been broken since GCC 3.0 (caused ICE) and we have now removed
   the extension.  See PR c/7353.  */

/* Case A: just the bare name = initializer.  */

typedef A = 0;  /* { dg-error "initialized" "A" } */
A a;            /* { dg-bogus "" "A error cascade" } */

/* Case B: with a type also.  */

typedef int B = 0;  /* { dg-error "initialized" "B" } */
B b;		    /* { dg-bogus "" "B error cascade" } */
