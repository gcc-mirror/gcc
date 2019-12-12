#include <stdlib.h>
#include <stddef.h>
#include <altivec.h>

#ifndef RTYPE
#define RTYPE TYPE
#endif

#ifdef DO_TRACE
#include <stdio.h>

#define TRACE(STRING, NUM)						\
do									\
  {									\
    fprintf (stderr, "%s: %2d\n", STRING, (int) NUM);			\
    fflush (stderr);							\
  }									\
while (0)

#ifndef FAIL_FORMAT
#define FAIL_FORMAT "%ld"
#define FAIL_CAST(X) ((long)(X))
#endif

#define FAIL(EXP, GOT)							 \
do									 \
  {									 \
    fprintf (stderr, "Expected: " FAIL_FORMAT ", got " FAIL_FORMAT "\n", \
	     FAIL_CAST (EXP), FAIL_CAST (GOT));				 \
    fflush (stderr);							 \
    abort ();								 \
  }									 \
while (0)

#else
#define TRACE(STRING, NUM)
#define FAIL(EXP, GOT) abort ()
#endif

static void
check (RTYPE, RTYPE) __attribute__((__noinline__));

static vector TYPE
deoptimize (vector TYPE) __attribute__((__noinline__));

static vector TYPE
*deoptimize_ptr (vector TYPE *)	__attribute__((__noinline__));

static void
check (RTYPE expected, RTYPE got)
{
  if (expected != got)
    FAIL (expected, got);
}

static vector TYPE
deoptimize (vector TYPE a)
{
  __asm__ (" # %x0" : "+v" (a));
  return a;
}

static vector TYPE *
deoptimize_ptr (vector TYPE *p)
{
  __asm__ (" # %0" : "+r" (p));
  return p;
}


RTYPE
get_auto_0 (vector TYPE a)
{
  TRACE ("get_auto_", 0);
  return (RTYPE) vec_extract (a, 0);
}

RTYPE
get_auto_1 (vector TYPE a)
{
  TRACE ("get_auto_", 1);
  return (RTYPE) vec_extract (a, 1);
}

#if ELEMENTS >= 4
RTYPE
get_auto_2 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 2);
}

RTYPE
get_auto_3 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 3);
}

#if ELEMENTS >= 8
RTYPE
get_auto_4 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 4);
}

RTYPE
get_auto_5 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 5);
}

RTYPE
get_auto_6 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 6);
}

RTYPE
get_auto_7 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 7);
}

#if ELEMENTS >= 16
RTYPE
get_auto_8 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 8);
}

RTYPE
get_auto_9 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 9);
}

RTYPE
get_auto_10 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 10);
}

RTYPE
get_auto_11 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 11);
}

RTYPE
get_auto_12 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 12);
}

RTYPE
get_auto_13 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 13);
}

RTYPE
get_auto_14 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 14);
}

RTYPE
get_auto_15 (vector TYPE a)
{
  return (RTYPE) vec_extract (a, 15);
}

#endif
#endif
#endif


/* Tests for the normal case of vec_extract where the vector is in a register
   and returning the result in a register as a return value.  */
#ifdef DISABLE_INLINE_OF_GET_AUTO_N
__attribute__ ((__noinline__))
#else
/* gcc issues warning: always_inline function might not be inlinable

   __attribute__ ((__always_inline__))
*/
#endif
RTYPE
get_auto_n (vector TYPE a, ssize_t n)
{
  return (RTYPE) vec_extract (a, n);
}

typedef RTYPE (*auto_func_type) (vector TYPE);

static auto_func_type get_auto_const[] = {
  get_auto_0,
  get_auto_1,
#if ELEMENTS >= 4
  get_auto_2,
  get_auto_3,
#if ELEMENTS >= 8
  get_auto_4,
  get_auto_5,
  get_auto_6,
  get_auto_7,
#if ELEMENTS >= 16
  get_auto_8,
  get_auto_9,
  get_auto_10,
  get_auto_11,
  get_auto_12,
  get_auto_13,
  get_auto_14,
  get_auto_15,
#endif
#endif
#endif
};

extern void do_auto (vector TYPE a) __attribute__((__noinline__));

void
do_auto (vector TYPE a)
{
  size_t i;

  for (i = 1; i < 40; i += 3)
    {
      TRACE ("do_auto, i: ", i);
      TRACE ("  get_auto_const[i] returns: ",
	     (*get_auto_const [i % ELEMENTS]) (a));
      TRACE ("  get_auto_n returns", get_auto_n (a, i));
      check (get_auto_n (a, i), (*get_auto_const [i % ELEMENTS]) (a));
    }
}



/* Main program to test all of the possibilities.  */
int
main (void)
{
  size_t i;
  vector TYPE x = INITIAL;
  vector TYPE *p, *p2, a, y;
  vector TYPE z[2];

  a = deoptimize (x);

  do_auto (a);

  return 0;
}
