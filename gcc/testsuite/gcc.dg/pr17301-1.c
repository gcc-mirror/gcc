/* Invalid use of __builtin_stdarg_start should not cause an ICE.  Bug
   17301.  */
/* { dg-do compile } */
/* { dg-options "" } */

int
write_format (char *format, ...)
{
  __builtin_va_list p;
  __builtin_stdarg_start (p); /* { dg-error "too few arguments to function 'va_start'" } */
}
