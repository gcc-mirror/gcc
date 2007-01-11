/* { dg-do compile } */
/* { dg-options "-Winline -O2" } */
void do_something_evil (void);
inline __attribute__ ((always_inline)) void
q2(void)
{ 				/* { dg-error "recursive" "" } */
  do_something_evil ();
  q2(); 			/* { dg-error "called from here" "" } */
  q2(); 			/* { dg-error "called from here" "" } */
}
