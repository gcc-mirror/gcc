// { dg-do run { target aarch64*-*-* } }
// { dg-require-effective-target dfp }
/* { dg-require-effective-target dfprt } */

/* Test unwinding of AArch64 register saves.  */
/* We cannot use #include <decimal/decimal> because it defines
   decimal* types as classes, which cannot be assigned to register
   variables.  Hence the use the mode attribute trick.  */

#ifdef __aarch64__

typedef float dec64 __attribute__((mode(DD)));

extern "C" void abort (void);
extern "C" void exit (int);

void
foo (void)
{
  register dec64 v10 asm("v10") = 0;
  register dec64 v11 asm("v11") = 1;
  register dec64 v12 asm("v12") = 2;
  register dec64 v13 asm("v13") = 3;
  asm volatile ("" : "+w" (v10), "+w" (v11), "+w" (v12), "+w" (v13));
  throw "";
}

int
main (void)
{
  register dec64 v10 asm("v10") = 10;
  register dec64 v11 asm("v11") = 11;
  register dec64 v12 asm("v12") = 12;
  register dec64 v13 asm("v13") = 13;
  asm volatile ("" : "+w" (v10), "+w" (v11), "+w" (v12), "+w" (v13));
  try {
    foo ();
  } catch (...) {
    asm volatile ("" : "+w" (v10), "+w" (v11), "+w" (v12), "+w" (v13));
    if (v10 != 10 || v11 != 11 || v12 != 12 || v13 != 13)
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
