/* Examples of a mismatching function pointer type in
   legacy code that got the wrong return type for the signal handler.

   Adapted from a52dec-0.7.4: src/a52dec.c which is GPLv2+.  */

/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

typedef void (*__sighandler_t) (int); /* { dg-message "'__sighandler_t' declared here" } */

extern __sighandler_t signal (int __sig, __sighandler_t __handler) /* { dg-message "expected '__sighandler_t' \\{aka '\[^\n\r\]*'\\} but argument is of type '\[^\n\r\]*'" } */
     __attribute__ ((__nothrow__ , __leaf__));

/* Mismatching return type.  */
#define RETSIGTYPE int
static RETSIGTYPE signal_handler (int sig) /* { dg-message "'signal_handler' declared here" } */
{
}

static void print_fps (int final)
{
  signal (42, signal_handler); /* { dg-error "passing argument 2 of 'signal' from incompatible pointer type" } */
}
