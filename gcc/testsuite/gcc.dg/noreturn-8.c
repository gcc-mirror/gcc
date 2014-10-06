/* { dg-do run { target nonpic } } */
/* { dg-options "-O2" } */
void exit (int);
void noreturn_autodetection_failed ();
__attribute__ ((noinline)) int
detect_noreturn ()
{
  exit (0);
}
int
main (void)
{
  detect_noreturn ();
  noreturn_autodetection_failed ();
  return 0;
}
