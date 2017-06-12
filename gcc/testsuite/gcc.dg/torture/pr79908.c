/* { dg-do compile } */

/* Used to fail in the stdarg pass before fix for PR79908.  */

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

void testva (int n, ...)
{
  va_list ap;
  _Complex int i = __builtin_va_arg (ap, _Complex int);
}
