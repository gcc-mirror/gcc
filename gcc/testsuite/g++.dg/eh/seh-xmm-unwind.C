/* PR target/99234 */
/* Test SEH unwinding of XMM register saves.  */

/* { dg-do run { target { x86_64-*-mingw32 && lp64 } } } */

extern "C" void abort (void);
extern "C" void exit (int);

void
foo (void)
{
  register __int128 xmm6  asm("xmm6")  = 0;
  register __int128 xmm7  asm("xmm7")  = 0;
  register __int128 xmm8  asm("xmm8")  = 0;
  register __int128 xmm9  asm("xmm9")  = 0;
  register __int128 xmm10 asm("xmm10") = 0;
  register __int128 xmm11 asm("xmm11") = 0;
  register __int128 xmm12 asm("xmm12") = 0;
  register __int128 xmm13 asm("xmm13") = 0;
  register __int128 xmm14 asm("xmm14") = 0;
  register __int128 xmm15 asm("xmm15") = 0;

  __asm__ __volatile__ ("" : "+x" (xmm6),  "+x" (xmm7),  "+x" (xmm8),  "+x" (xmm9),
			     "+x" (xmm10), "+x" (xmm11), "+x" (xmm12), "+x" (xmm13),
			     "+x" (xmm14), "+x" (xmm15));

  throw 1;
}

int
main (void)
{
  register __int128 xmm6  asm("xmm6")  = 6;
  register __int128 xmm7  asm("xmm7")  = 7;
  register __int128 xmm8  asm("xmm8")  = 8;
  register __int128 xmm9  asm("xmm9")  = 9;
  register __int128 xmm10 asm("xmm10") = 10;
  register __int128 xmm11 asm("xmm11") = 11;
  register __int128 xmm12 asm("xmm12") = 12;
  register __int128 xmm13 asm("xmm13") = 13;
  register __int128 xmm14 asm("xmm14") = 14;
  register __int128 xmm15 asm("xmm15") = 15;

  __asm__ __volatile__ ("" : "+x" (xmm6),  "+x" (xmm7),  "+x" (xmm8),  "+x" (xmm9),
			     "+x" (xmm10), "+x" (xmm11), "+x" (xmm12), "+x" (xmm13),
			     "+x" (xmm14), "+x" (xmm15));

  try {
    foo ();
  } catch (...) {
    __asm__ __volatile__ ("" : "+x" (xmm6),  "+x" (xmm7),  "+x" (xmm8),  "+x" (xmm9),
			       "+x" (xmm10), "+x" (xmm11), "+x" (xmm12), "+x" (xmm13),
			       "+x" (xmm14), "+x" (xmm15));

    if (xmm6 != 6 || xmm7 != 7 || xmm8 != 8 || xmm9 != 9 || xmm10 != 10
        || xmm11 != 11 || xmm12 != 12 || xmm13 != 13 || xmm14 != 14 || xmm15 != 15)
      abort ();
  }

  exit (0);
}
