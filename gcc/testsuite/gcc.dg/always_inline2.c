/* { dg-do compile } */
/* { dg-options "-Winline -O2" } */
inline __attribute__ ((always_inline)) void t(void); /* { dg-message "sorry\[^\n\]*body not available" "" } */
void
q(void)
{
  t(); 				/* { dg-message "sorry\[^\n\]*called from here" "" } */
}
