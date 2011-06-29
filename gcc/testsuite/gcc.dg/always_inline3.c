/* { dg-do compile } */
/* { dg-options "-O2" } */
int do_something_evil (void);
inline __attribute__ ((always_inline)) void
q2(void) /* { dg-error "recursive inlining" } */
{
  if (do_something_evil ())
    return;
  q2(); 			/* { dg-error "called from here" } */
  q2(); /* With -O2 we don't warn here, it is eliminated by tail recursion.  */
}
