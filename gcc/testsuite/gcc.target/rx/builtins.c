/* { dg-do run } */

/* Verify that the RX specific builtin functions work.  */
   
#include <stdlib.h>
#include <stdio.h>

/* We need to prevent these functions from being inlined
   as otherwise gcc will attempt to optimize away their
   arguments and we need the operations on them in order
   to correctly set the psw flags.  */

int saturate_add         (int, int)      __attribute__((__noinline__));
int exchange             (int, int)      __attribute__((__noinline__));

int
half_word_swap (int arg)
{
  return __builtin_rx_revw (arg);
}

long
multiply_and_accumulate (long arg1, long arg2, long arg3)
{
  __builtin_rx_mvtaclo (0);
  __builtin_rx_mvtachi (0);

  __builtin_rx_mullo (arg1, arg2);
  __builtin_rx_mulhi (arg1, arg2);
  __builtin_rx_maclo (arg1, arg3);
  __builtin_rx_machi (arg1, arg3);

  __builtin_rx_racw (1);
  
  arg1 = __builtin_rx_mvfachi ();
  arg1 += __builtin_rx_mvfacmi ();

  return arg1;
}

int
rxround (float arg)
{
  return __builtin_rx_round (arg);
}

/* #define DEBUG 1 */

#ifdef DEBUG
#define CHECK_0ARG(func, result)					\
  if (func () != result)						\
    {									\
      printf (#func " () fails: %x not %x\n", func (), result);		\
      abort ();								\
    }

#define CHECK_1ARG(func, arg, result)					\
  if (func (arg) != result)						\
    {									\
      printf (#func " (" #arg ") fails: %x not %x\n", func (arg), result); \
      abort ();								\
    }

#define CHECK_2ARG(func, arg1, arg2, result)				\
  if (func (arg1, arg2) != result)					\
    {									\
      printf (#func " (" #arg1 "," #arg2 ") fails: %x not %x\n",	\
	      func (arg1, arg2), result);				\
      abort ();								\
    }

#define CHECK_3ARG(func, arg1, arg2, arg3, result)			\
  if (func (arg1, arg2, arg3) != result)				\
    {									\
      printf (#func " (" #arg1 "," #arg2 "," #arg3 ") fails: %x not %x\n",	\
	      func (arg1, arg2, arg3), result);				\
      abort ();								\
    }
#else
#define CHECK_0ARG(func, result)					\
  if (func () != result)						\
    abort ();

#define CHECK_1ARG(func, arg, result)					\
  if (func (arg) != result)						\
    abort ();

#define CHECK_2ARG(func, arg1, arg2, result)				\
  if (func (arg1, arg2) != result)					\
    abort ();

#define CHECK_3ARG(func, arg1, arg2, arg3, result)			\
  if (func (arg1, arg2, arg3) != result)				\
    abort ();
#endif

int
main (void)
{
  CHECK_1ARG (half_word_swap, 0x12345678, 0x34127856);
  CHECK_3ARG (multiply_and_accumulate, 0x111, 0x222, 0x333, 0x70007);
  CHECK_1ARG (rxround, 0.5, 1);
  return 0;
}

/* The following builtins are compiled but
   not executed because they need OS support.  */

void
rxbreak (void)
{
  __builtin_rx_brk ();
}

void
interrupt (void)
{
  __builtin_rx_int (0x12);
}

int
get_stack_pointer (void)
{
  return __builtin_rx_mvfc (2);
}

void
set_stack_pointer (int value)
{
  __builtin_rx_mvtc (2, value);
  __builtin_rx_mvtc (2, 0x1234);
}

void
wait (void)
{
  __builtin_rx_wait ();
}

void
rmpa (int * multiplicand, int * multiplier, int num)
{
  __builtin_rx_rmpa ();
}
