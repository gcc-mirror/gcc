/* { dg-do compile } */

/* Check that, when strub is not supported (so no dg-required-effective-target
   strub above), we report when strub functions that are not defined are
   called.  This cannot be part of strub-unsupported-2.c because errors in the
   strub-mode pass prevent the main strub pass, where errors at calls are
   detected, from running.  */

extern void __attribute__ ((__strub__))
s (void); /* { dg-message "not eligible|requested" "" { target { ! strub } } } */

extern void __attribute__ ((__strub__ ("internal")))
t (void); /* { dg-message "not eligible|requested" "" { target { ! strub } } } */

void m () {
  s ();
  t ();
}
