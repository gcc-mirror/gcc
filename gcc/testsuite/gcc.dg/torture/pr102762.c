/* { dg-do compile } */
/* We fail to diagnose the invalid __builtin_va_arg_pack use with -flto.  */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

void log_bad_request();
void foo(a, b)
     int a, b;
{
  log_bad_request(0, __builtin_va_arg_pack());  /* { dg-error "invalid use" } */
  foo(0);
}
