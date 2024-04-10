/* { dg-do compile } */

/* Check that, when strub is not supported (so no dg-required-effective-target
   strub above), we report when strub functions are defined, and when they're
   called in ways that would require changes.  */

void __attribute__ ((__strub__))
f (void) {} /* { dg-message "not eligible|requested" "" { target { ! strub } } } */

void __attribute__ ((__strub__ ("internal")))
g (void) {} /* { dg-message "not eligible|requested" "" { target { ! strub } } } */

/* This only gets an error when called, see strub-unsupported-2.c.  */
void __attribute__ ((__strub__ ("at-calls"))) (*p) (void);

/* These too, see strub-unsupported-3.c.  */
extern void __attribute__ ((__strub__))
s (void);

extern void __attribute__ ((__strub__ ("internal")))
t (void);
