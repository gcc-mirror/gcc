/* Test unwinding of iWMMXt register saves.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */

/* { dg-do run } */
/* { dg-require-effective-target arm32 } */

#ifdef __IWMMXT__
extern "C" void abort (void);
extern "C" void exit (int);

void
foo (void)
{
  register long long wr10 asm("wr10") = 0;
  register long long wr11 asm("wr11") = 1;
  register long long wr12 asm("wr12") = 2;
  register long long wr14 asm("wr14") = 4;
  register long long wr15 asm("wr15") = 5;
  asm volatile ("" : "+y" (wr10), "+y" (wr11), "+y" (wr12), "+y" (wr14), "+y" (wr15));
  throw "";
}

int
main (void)
{
  register long long wr10 asm("wr10") = 10;
  register long long wr11 asm("wr11") = 11;
  register long long wr12 asm("wr12") = 12;
  register long long wr13 asm("wr13") = 13;
  register long long wr14 asm("wr14") = 14;
  register long long wr15 asm("wr15") = 15;
  asm volatile ("" : "+y" (wr10), "+y" (wr11), "+y" (wr12), "+y" (wr13), "+y" (wr14), "+y" (wr15));
  try {
    foo ();
  } catch (...) {
    asm volatile ("" : "+y" (wr10), "+y" (wr11), "+y" (wr12), "+y" (wr13), "+y" (wr14), "+y" (wr15));
    if (wr10 != 10 || wr11 != 11 || wr12 != 12 || wr13 != 13 || wr14 != 14 || wr15 != 15)
      abort ();
  }
  exit (0);
}
#else
int
main (void)
{
}
#endif
