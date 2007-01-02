/* ICE in subreg_get_info: bug 30311.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
inline double bar(double x)
{
  long double d;
  __asm__ ("" : "=t" (d) : "0" (x));
  return d;
}

double foo(double x)
{
  if (x)
    return bar(x);
  else
    return bar(x);
}
