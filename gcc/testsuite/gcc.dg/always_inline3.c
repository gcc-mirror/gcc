/* { dg-do compile } */
/* { dg-options "-Winline -O2" } */
inline __attribute__ ((always_inline)) void
q2(void)
{ 				/* { dg-error "recursive" "" } */
  q2(); 			/* { dg-error "called from here" "" } */
  q2(); 			/* { dg-error "called from here" "" } */
}
