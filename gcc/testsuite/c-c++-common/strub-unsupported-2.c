/* { dg-do compile } */

/* Check that, when strub is not supported (so no dg-required-effective-target
   strub above), we report when pointers to strub functions are called.  This
   cannot be part of strub-unsupported.c because errors in the strub-mode pass
   prevent the main strub pass, where errors at calls are detected, from
   running.  */

void __attribute__ ((__strub__ ("at-calls"))) (*p) (void);

void m () {
  p (); /* { dg-message "unsupported" "" { target { ! strub } } } */
}
