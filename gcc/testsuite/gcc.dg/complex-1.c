/* { dg-do run } */
/* { dg-options "-O" } */

/* Verify that the 6th complex floating-point argument is
   correctly passed as unnamed argument on SPARC64.  */

extern void abort(void);   

void foo(long arg1, long arg2, long arg3, long arg4, long arg5, ...)
{
  __builtin_va_list ap;
  _Complex float cf;

  __builtin_va_start(ap, arg5);
  cf = __builtin_va_arg(ap, _Complex float);
  __builtin_va_end(ap);

  if (__imag__ cf != 2.0f)
    abort();
}

int bar(long arg1, long arg2, long arg3, long arg4, long arg5, _Complex float arg6)
{
  foo(arg1, arg2, arg3, arg4, arg5, arg6);
  return 0;
}

int main(void)
{
  return bar(0, 0, 0, 0, 0, 2.0fi);
}
