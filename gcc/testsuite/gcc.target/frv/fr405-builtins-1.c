/* Test the IACC multiply/accumulate instructions.  Also test the IACC
   read/write functions.  */
/* { dg-options "-mcpu=fr405" } */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

int main ()
{
  long long res, res1, res2, res3;

  __SMU (0x12345678, 0x40004000);
  __SMASS (0x12000000, 0x11223344);
  __SMSSS (0x01020304, 0x54321000);

  res = __IACCreadll (0);
  res1 = 0x12345678LL * 0x40004000LL;
  res2 = 0x12000000LL * 0x11223344LL;
  res3 = 0x01020304LL * 0x54321000LL;
  if (res != res1 + res2 - res3)
    abort ();

  __IACCsetll (0, 0x7ffffffffffffff0LL);
  __SMASS (0x100, 0x100);
  if (__IACCreadll (0) != 0x7fffffffffffffffLL)
    abort ();

  __IACCsetl (0, -0x7ffffffe);
  __IACCsetl (1, 0);
  __SMSSS (0x10001, 0x10000);
  if (__IACCreadl (0) != -0x7fffffff - 1 || __IACCreadl (1) != -0x10000)
    abort ();

  __SMSSS (0x10001, 0x10000);
  if (__IACCreadl (0) != -0x7fffffff - 1 || __IACCreadl (1) != 0)
    abort ();

  exit (0);
}
