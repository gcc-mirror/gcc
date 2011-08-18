/* Test C1X _Noreturn.  Test _Noreturn on main, hosted.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors -fhosted" } */

_Noreturn void exit (int);

_Noreturn int
main (void) /* { dg-error "'main' declared '_Noreturn'" } */
{
  exit (0);
}
