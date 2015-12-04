/* { dg-do run } */
/* { dg-options "-std=c99" } */

extern int printf (const char *, ...);
extern void abort (void) __attribute__((noreturn));

typedef unsigned long uint32;
typedef unsigned long long uint64;

extern uint32 __mspabi_divul (uint32, uint32);
extern uint32 __mspabi_divlu (uint32, uint32);
extern uint64 __mspabi_divull (uint64, uint64);
extern uint64 __mspabi_divllu (uint64, uint64);

uint32 func1 (uint32, uint32) __attribute__ ((noinline));
uint32 func2 (uint32, uint32) __attribute__ ((noinline));
uint32 func3 (uint32, uint32) __attribute__ ((noinline));
uint64 func4 (uint64, uint64) __attribute__ ((noinline));
uint64 func5 (uint64, uint64) __attribute__ ((noinline));
uint64 func6 (uint64, uint64) __attribute__ ((noinline));


#define DEBUG 0

int
main (void)
{
  int fail = 0;

  if (func1 (7UL, 3UL) != 2UL)
    {
#if DEBUG
      printf ("FAIL: func1: 7 / 3 returns %lu\n", func1 (7UL, 3UL));
#endif
      ++ fail;
    }

  if (func2 (7UL, 3UL) != 2UL)
    {
#if DEBUG
      printf ("FAIL: func2: 7 / 3 returns %lu\n", func2 (7UL, 3UL));
#endif
      ++ fail;
    }

  if (func3 (7UL, 3UL) != 2UL)
    {
#if DEBUG
      printf ("FAIL: func4: 7 / 3 returns %lu\n", func3 (7UL, 3UL));
#endif
      ++ fail;
    }

  if (func4 (7ULL, 3ULL) != 2ULL)
    {
#if DEBUG
      printf ("FAIL: func4: 7 / 3 returns %llu\n", func4 (7ULL, 3ULL));
#endif
      ++ fail;
    }

  if (func5 (7ULL, 3ULL) != 2ULL)
    {
#if DEBUG
      printf ("FAIL: func5: 7 / 3 returns %llu\n", func5 (7ULL, 3ULL));
#endif
      ++ fail;
    }

  if (func6 (7ULL, 3ULL) != 2ULL)
    {
#if DEBUG
      printf ("FAIL: func6: 7 / 3 returns %llu\n", func6 (7ULL, 3ULL));
#endif
      ++ fail;
    }

  if (fail)
    abort ();

  return 0;
}

/* At high levels of optimization gcc will probably fold func1 and func4 into
   main, but this does not really matter.  Those two functions are just there
   for a sanity check at low levels of optimization.  */
			      
uint32 func1 (uint32 a, uint32 b) { return a / b; }
uint32 func2 (uint32 a, uint32 b) { return __mspabi_divul (a, b); }
uint32 func3 (uint32 a, uint32 b) { return __mspabi_divlu (a, b); }
uint64 func4 (uint64 a, uint64 b) { return a / b; }
uint64 func5 (uint64 a, uint64 b) { return __mspabi_divull (a, b); }

uint64
func6 (uint64 a, uint64 b)
{
  uint64 ret;

  /* This test function is special.  The correctly spelt ABI function
     __mspabi_divull takes its first argument in registers R8::R11 and its
     second argument in registers R12::R15, but GCC knows that __mspabi_divllu
     is not the correct spelling and so it will use the normal function
     calling convention - first argument in R12::R15, second argument on the
     stack.
     
     The stub function for __mspabi_divllu in libgcc just does a BRAnch to
     the real __mspabi_divull function - it does *not* rearrange the arguments
     or pull anything off the stack.  This is correct, because in real code
     that calls __mspabi_divllu, compiled by *old* versions of gcc, the
     arguments will already be in the special ABI mandated locations.

     As a result, in order to test __mspabi_divllu here, we have to put the
     arguments into the correct registers ourselves and call __mspabi_divllu
     manually.  This does lead to some very inefficient code generation, but
     that is not our concern here.  */

#ifdef __MSP430X_LARGE__
  __asm ("mov  %A1, r8\n\
        mov  %B1, r9\n\
        mov  %C1, r10\n\
        mov  %D1, r11\n\
        mov  %A2, r12\n\
        mov  %B2, r13\n\
        mov  %C2, r14\n\
        mov  %D2, r15\n\
        calla #__mspabi_divllu\n\
        mov  r12, %A0\n\
        mov  r13, %B0\n\
        mov  r14, %C0\n\
        mov  r15, %D0\n"
	     : "=r" (ret) : "r" (a), "m" (b));
#else
  __asm ("mov  %A1, r8\n\
        mov  %B1, r9\n\
        mov  %C1, r10\n\
        mov  %D1, r11\n\
        mov  %A2, r12\n\
        mov  %B2, r13\n\
        mov  %C2, r14\n\
        mov  %D2, r15\n\
        call #__mspabi_divllu\n\
        mov  r12, %A0\n\
        mov  r13, %B0\n\
        mov  r14, %C0\n\
        mov  r15, %D0\n"
	     : "=r" (ret) : "r" (a), "m" (b));
#endif

  return ret;
}
