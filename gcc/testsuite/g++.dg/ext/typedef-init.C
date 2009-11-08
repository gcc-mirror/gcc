/* { dg-do compile } */
/* { dg-options "-fpermissive" } // suppress default -pedantic-errors */

/* This code used to be a legitimate, if dubious, extension.  However,
   it's been broken since GCC 3.0 (caused ICE) and we have now removed
   the extension.  See PR c/7353.

   For cases A and C, C++ issues a warning in addition to the error,
   since this construct appears to be a case of implicit int
   (forbidden in std. C++) until we get to the equals sign.  */

/* Case A: just the bare name = initializer.  */

typedef A = 0;  /* { dg-error "does not name a type" "A" } */
A a;            /* { dg-error "does not name a type" "A error cascade" } */

/* Case B: with a type also.  */

typedef int B = 0;  /* { dg-error "initialized" "B" } */
B b;		    /* { dg-error "does not name a type" "B error cascade" } */

/* C and D are the same as A and B, but wrapped in a structure;
   field declarations go by a different code path in C++ (ick).  */

struct S {
  typedef C = 0; /* { dg-error "does not name a type" "C" } */
  C c;		 /* { dg-error "" "C error cascade" } */

  typedef int D = 0; /* { dg-error "initialized" "D" } */
  D d;		     /* { dg-bogus "" "D error cascade" } */
};

template<int> void foo()
{
    typedef int i = 0; /* { dg-error "is initialized" } */
}
