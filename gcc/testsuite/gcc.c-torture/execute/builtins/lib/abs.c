extern int inside_main;
extern void abort (void);
#ifdef __OPTIMIZE__
#define ABORT_INSIDE_MAIN do { if (inside_main) abort (); } while (0)
#else
#define ABORT_INSIDE_MAIN do { } while (0)
#endif

typedef __INTMAX_TYPE__ intmax_t;
typedef unsigned __INTMAX_TYPE__ uintmax_t;

__attribute__ ((__noinline__))
int
abs (int x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -x : x;
}

__attribute__ ((__noinline__))
long
labs (long x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -x : x;
}

__attribute__ ((__noinline__))
long long
llabs (long long x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -x : x;
}

__attribute__ ((__noinline__))
intmax_t
imaxabs (intmax_t x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -x : x;
}

__attribute__ ((__noinline__))
unsigned int
uabs (int x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -(unsigned int) x : x;
}

__attribute__ ((__noinline__))
unsigned long
ulabs (long x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -(unsigned long) x : x;
}

__attribute__ ((__noinline__))
unsigned long long
ullabs (long long x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -(unsigned long long) x : x;
}

__attribute__ ((__noinline__))
uintmax_t
uimaxabs (intmax_t x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -(uintmax_t) x : x;
}
