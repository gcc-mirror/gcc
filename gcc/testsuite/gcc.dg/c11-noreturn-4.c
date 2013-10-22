/* Test C11 _Noreturn.  Test _Noreturn on main, freestanding.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -ffreestanding" } */

_Noreturn void exit (int);

_Noreturn int
main (void)
{
  exit (0);
}
