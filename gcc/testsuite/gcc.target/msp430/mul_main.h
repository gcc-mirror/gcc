extern void abort (void) __attribute__((noreturn));
extern int printf (const char *, ...);

int func1 (int, int) __attribute__((noinline));
long func2 (int, int)  __attribute__((noinline));
long func3 (long, long)  __attribute__((noinline));
long long func4 (long, long)  __attribute__((noinline));
long long func5 (long long, long long)  __attribute__((noinline));
unsigned long func6 (unsigned int, unsigned int)  __attribute__((noinline));
unsigned long long func7 (unsigned long, unsigned long)  __attribute__((noinline));

#define DEBUG 0

int
main (void)
{
  if (func1 (6, 7) != 42)
    {
#if DEBUG
      printf ("test1 fail: 6 * 7 = %d!\n", func1 (6, 7));
#endif
      abort ();
    }

  if (func2 (1000, 1000) != 1000000L)
    {
#if DEBUG
      printf ("test2 fail: 1000 * 1000 = %ld!\n", func2 (1000, 1000));
#endif
      abort ();
    }

  if (func3 (70000L, -32L) != -2240000L)
    {
#if DEBUG
      printf ("test3 fail: 70000 * -32 = %ld!\n", func3 (70000L, -32L));
#endif
      abort ();
    }

  if (func4 (-40L, -80L) != 3200LL)
    {
#if DEBUG
      printf ("test4 fail: -40 * -80 = %lld!\n", func4 (-40L, -80L));
#endif
      abort ();
    }

  if (func5(9LL, 9LL) != 81LL)
    {
#if DEBUG
      printf ("test5 fail: 9 * 9 = %lld!\n", func5 (9LL, 9LL));
#endif
      abort ();
    }

  if (func6 (-2U, 8U) != 524272LU)
    {
#if DEBUG
      printf ("test6 fail: -2 * 8 = %lu!\n", func6 (-2U, 8U));
#endif
      abort ();
    }

  if (func7 (99UL, 101UL) != 9999LLU)
    {
#if DEBUG
      printf ("test7 fail: 99 * 101 = %llu!\n", func7 (99UL, 101UL));
#endif
      abort ();
    }
  
  return 0;
}

int func1 (int a, int b) { return a * b; }
long func2 (int a, int b) { return (long) a * (long) b; }
long func3 (long a, long b) { return a * b; }
long long func4 (long a, long b) { return (long long) a * (long long) b; }
long long func5 (long long a, long long b) { return a * b; }
unsigned long func6 (unsigned int a, unsigned int b) { return (unsigned long) a * (unsigned long) b; }
unsigned long long func7 (unsigned long a, unsigned long b) { return (unsigned long long) a * (unsigned long long) b; }

