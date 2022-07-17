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
/* { dg-error "17:missing terminating" "" { target *-*-* } .-1 } */
/* { dg-warning "expected a string" "" { target *-*-* } .-2 } */
#pragma message "Bad 1
/* { dg-error "17:missing terminating" "" { target *-*-* } .-1 } */
/* { dg-warning "expected a string" "" { target *-*-* } .-2 } */
#pragma message ("Bad 2
/* { dg-error "18:missing terminating" "" { target *-*-* } .-1 } */
/* { dg-warning "expected a string" "" { target *-*-* } .-2 } */
#pragma message ("Bad 3"
/* { dg-warning "malformed '#pragma message" "" { target *-*-* } .-1 } */

#pragma message "" junk
/* { dg-warning "junk at end of '#pragma message'" "" { target *-*-* } .-1 } */

#pragma message ("") junk
/* { dg-warning "junk at end of '#pragma message'" "" { target *-*-* } .-1 } */

#pragma message ""               /* No output expected for empty messages.  */
#pragma message ("")

#pragma message "Okay 1"         /* { dg-message "Okay 1" } */
#pragma message ("Okay 2")       /* { dg-message "Okay 2" } */
#define THREE "3"
#pragma message ("Okay " THREE)  /* { dg-message "Okay 3" } */

/* Create a TODO() that prints a message on compilation.  */
#define DO_PRAGMA(x) _Pragma (#x) /* { dg-line pragma_loc1 } */
#define TODO(x) DO_PRAGMA(message ("TODO - " #x)) /* { dg-line pragma_loc2 } */
TODO(Okay 4) /* { dg-message "in expansion of macro 'TODO'" } */
/* { dg-message "TODO - Okay 4" "test4.1" { target *-*-* } pragma_loc1 } */
/* { dg-message "in expansion of macro 'DO_PRAGMA'" "test4.2" { target *-*-* } pragma_loc2 } */

#if 0
#pragma message ("Not printed")
#endif

int unused;  /* Silence `ISO C forbids an empty translation unit' warning.  */
