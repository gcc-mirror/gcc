/* Test for cross x86_64<->w64 abi standard calls.  */
/* { dg-do run { target i?86-*-linux* x86_64-*-linux* } } */
/* { dg-options "-O2 -mabi=ms -std=gnu99 -ffast-math -fno-builtin -maccumulate-outgoing-args" } */
/* { dg-additional-sources "func-2b.c" } */

extern void __attribute__ ((sysv_abi)) abort (void);
long double func_cross (long double, double, float, long, int, char);

long double __attribute__ ((sysv_abi))
func_native (long double a, double b, float c, long d, int e, char f)
{
  long double ret;
  ret = a + (long double) b + (long double) c;
  ret *= (long double) (d + (long) e);
  if (f>0)
    ret += func_native (a,b,c,d,e,-f);
  return ret;
}

int __attribute__ ((sysv_abi))
main ()
{
  if (func_cross (1.0,2.0,3.0,1,2,3)
      != func_native (1.0,2.0,3.0,1,2,3))
    abort ();
  return 0;
}
