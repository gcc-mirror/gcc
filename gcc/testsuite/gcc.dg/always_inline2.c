/* { dg-do compile } */
/* { dg-options "-O2" } */
inline __attribute__ ((always_inline)) void t(void); /* { dg-error "body not available" } */
void
q(void)
{
  t(); 				/* { dg-error "called from here" } */
}
