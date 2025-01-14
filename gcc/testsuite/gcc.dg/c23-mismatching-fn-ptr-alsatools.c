/* Examples of a mismatching function pointer type in
   legacy code that failed to compile with C23.

   Adapted from alsa-tools-1.2.11:envy24control/profiles.c which is GPLv2+.  */

/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

typedef void (*__sighandler_t) (int); /* { dg-message "'__sighandler_t' declared here" } */

extern __sighandler_t signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__ , __leaf__));

void new_process(void)
{
  void (*int_stat)();

  int_stat = signal(2,  ((__sighandler_t) 1)); /* { dg-error "assignment to '\[^\n\r\]*' from incompatible pointer type '__sighandler_t' \\{aka '\[^\n\r\]*'\\}" } */

  signal(2, int_stat); /* { dg-error "passing argument 2 of 'signal' from incompatible pointer type" } */
}
