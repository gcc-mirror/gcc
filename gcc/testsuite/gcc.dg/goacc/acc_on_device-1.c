/* Have to enable optimizations, as otherwise builtins won't be expanded.  */
/* { dg-additional-options "-O -fdump-rtl-expand -std=c89 -Wno-implicit-function-declaration" } */

int
f (void)
{
  int r = 0;

  r |= acc_on_device ();
  r |= acc_on_device (1, 2);
  r |= acc_on_device (3.14);
  r |= acc_on_device ("hello");

  return r;
}

/* Unsuitable to be handled as a builtin, so we're expecting four calls.
   { dg-final { scan-rtl-dump-times "\\\(call \[^\\n\]* acc_on_device" 4 "expand" } } */

