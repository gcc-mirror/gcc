/* Used to be miscompiled at -O0 due to incorrect choice of sign extension
   vs. zero extension.  __attribute__ ((noinline)) added to try to make it
   fail at higher optimization levels too.  */

extern void abort (void);

long long __attribute__ ((noinline))
div (long long val)
{
  return val / 32768;
}

int main (void)
{
  long long d1 = -990000000;
  long long d2 = div(d1);
  if (d2 != -30212)
    abort ();
  return 0;
}

