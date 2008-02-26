/* Invalid use of __builtin_va_start should not cause an ICE.  Bug
   17301.  Case with no arguments.  */
/* { dg-do compile } */
/* { dg-options "" } */

void foo (char *format, ...)
{
  __builtin_va_start (); /* { dg-error "too few arguments to function '__builtin_va_start'" } */
}
