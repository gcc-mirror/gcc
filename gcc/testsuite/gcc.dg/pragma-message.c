/* Test that #pragma message "..." writes compiler messages. */

#pragma message                  /* { dg-warning "expected a string" } */
#pragma message 0                /* { dg-warning "expected a string" } */
#pragma message id               /* { dg-warning "expected a string" } */
#pragma message (                /* { dg-warning "expected a string" } */
#pragma message (0               /* { dg-warning "expected a string" } */
#pragma message (id              /* { dg-warning "expected a string" } */
#pragma message ()               /* { dg-warning "expected a string" } */
#pragma message (0)              /* { dg-warning "expected a string" } */
#pragma message (id)             /* { dg-warning "expected a string" } */

/* gcc prefixes '#pragma message ...' output with filename and line number,
   then 'note: #pragma message: ', allowing dg-message to check output.
   If unexpected pragma messages are printed (anything not caught by a
   matching dg-message), dejagnu will report these as excess errors.  */

#pragma message "
/* { dg-error "missing terminating" "" { target *-*-* } 18 } */
/* { dg-warning "expected a string" "" { target *-*-* } 18 } */
#pragma message "Bad 1
/* { dg-error "missing terminating" "" { target *-*-* } 21 } */
/* { dg-warning "expected a string" "" { target *-*-* } 21 } */
#pragma message ("Bad 2
/* { dg-error "missing terminating" "" { target *-*-* } 24 } */
/* { dg-warning "expected a string" "" { target *-*-* } 24 } */
#pragma message ("Bad 3"
/* { dg-warning "malformed '#pragma message" "" { target *-*-* } 27 } */

#pragma message "" junk
/* { dg-warning "junk at end of '#pragma message'" "" { target *-*-* } 30 } */

#pragma message ("") junk
/* { dg-warning "junk at end of '#pragma message'" "" { target *-*-* } 33 } */

#pragma message ""               /* No output expected for empty messages.  */
#pragma message ("")

#pragma message "Okay 1"         /* { dg-message "Okay 1" } */
#pragma message ("Okay 2")       /* { dg-message "Okay 2" } */
#define THREE "3"
#pragma message ("Okay " THREE)  /* { dg-message "Okay 3" } */

/* Create a TODO() that prints a message on compilation.  */
#define DO_PRAGMA(x) _Pragma (#x)
#define TODO(x) DO_PRAGMA(message ("TODO - " #x))
TODO(Okay 4)                     /* { dg-message "TODO - Okay 4" } */

#if 0
#pragma message ("Not printed")
#endif

int unused;  /* Silence `ISO C forbids an empty translation unit' warning.  */
