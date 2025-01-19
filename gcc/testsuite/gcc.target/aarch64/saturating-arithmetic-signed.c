/* { dg-do run } */
/* { dg-options "-O2 --save-temps -mearly-ra=none -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <limits.h>
#include <stdbool.h>
#include <stdint.h>

/*
** sadd32:
**	asr	w([0-9]+), w1, 31
**	eor	w\1, w\1, -2147483648
**	adds	w([0-9]+), (?:w0, w1|w1, w0)
**	csinv	w0, w\2, w\1, vc
**	ret
*/
int32_t __attribute__((noipa))
sadd32 (int32_t __a, int32_t __b)
{
  int32_t sum;
  bool overflow = __builtin_add_overflow (__a, __b, &sum);
  return !overflow ? sum : __a < 0 ? INT_MIN : INT_MAX;
}

/*
** sadd32_imm:
**	adds	w([0-9]+), w0, #67
**	mov	w([0-9]+), 2147483647
**	csel	w0, w\1, w\2, vc
**	ret
*/
int32_t __attribute__((noipa))
sadd32_imm (int32_t __a)
{
  int32_t sum;
  bool overflow = __builtin_add_overflow (__a, 67, &sum);
  return !overflow ? sum : __a < 0 ? INT_MIN : INT_MAX;
}

/*
** sadd32_imm2:
**	subs	w([0-9]+), w0, 67
**	mov	w([0-9]+), -2147483648
**	csel	w0, w\1, w\2, vc
**	ret
*/
int32_t  __attribute__((noipa))
sadd32_imm2 (int32_t __a)
{
  int32_t sum;
  bool overflow = __builtin_add_overflow (__a, -67, &sum);
  return !overflow ? sum : __a < 0 ? INT_MIN : INT_MAX;
}

/*
** ssub32:
**	asr	w([0-9]+), w1, 31
**	eor	w\1, w\1, -2147483648
**	subs	w([0-9]+), w0, w1
**	csel	w0, w\2, w\1, vc
**	ret
*/
int32_t  __attribute__((noipa))
ssub32 (int32_t __a, int32_t __b)
{
  int32_t result;
  bool overflow = __builtin_sub_overflow (__a, __b, &result);
  return !overflow ? result : __a < 0 ? INT_MIN : INT_MAX;
}

/*
** ssub32_imm:
**	subs	w([0-9]+), w0, 67
**	mov	w([0-9]+), -2147483648
**	csel	w0, w\1, w\2, vc
**	ret
*/
int32_t  __attribute__((noipa))
ssub32_imm (int32_t __a)
{
  int32_t result;
  bool overflow = __builtin_sub_overflow (__a, 67, &result);
  return !overflow ? result : __a < 0 ? INT_MIN : INT_MAX;
}

/*
** ssub32_imm2:
**	adds	w([0-9]+), w0, #67
**	mov	w([0-9]+), 2147483647
**	csel	w0, w\1, w\2, vc
**	ret
*/
int32_t  __attribute__((noipa))
ssub32_imm2 (int32_t __a)
{
  int32_t result;
  bool overflow = __builtin_sub_overflow (__a, -67, &result);
  return !overflow ? result : __a < 0 ? INT_MIN : INT_MAX;
}

/*
** sadd64:
**	asr	x([0-9]+), x1, 63
**	eor	x\1, x\1, -9223372036854775808
**	adds	x([0-9]+), (?:x0, x1|x1, x0)
**	csinv	x0, x\2, x\1, vc
**	ret
*/
int64_t  __attribute__((noipa))
sadd64 (int64_t __a, int64_t __b)
{
  int64_t sum;
  bool overflow = __builtin_add_overflow (__a, __b, &sum);
  return !overflow ? sum : __a < 0 ? LONG_MIN : LONG_MAX;
}

/*
** sadd64_imm:
**	adds	x([0-9]+), x0, #67
**	mov	x([0-9]+), 9223372036854775807
**	csel	x0, x\1, x\2, vc
**	ret
*/
int64_t  __attribute__((noipa))
sadd64_imm (int64_t __a)
{
  int64_t sum;
  bool overflow = __builtin_add_overflow (__a, (int64_t)67, &sum);
  return !overflow ? sum : __a < 0 ? LONG_MIN : LONG_MAX;
}

/*
** sadd64_imm2:
**	subs	x([0-9]+), x0, 67
**	mov	x([0-9]+), -9223372036854775808
**	csel	x0, x\1, x\2, vc
**	ret
*/
int64_t  __attribute__((noipa))
sadd64_imm2 (int64_t __a)
{
  int64_t sum;
  bool overflow = __builtin_add_overflow (__a, (int64_t)-67, &sum);
  return !overflow ? sum : __a < 0 ? LONG_MIN : LONG_MAX;
}

/*
** ssub64:
**	asr	x([0-9]+), x1, 63
**	eor	x\1, x\1, -9223372036854775808
**	subs	x([0-9]+), x0, x1
**	csel	x0, x\2, x\1, vc
**	ret
*/
int64_t  __attribute__((noipa))
ssub64 (int64_t __a, int64_t __b)
{
  int64_t result;
  bool overflow = __builtin_sub_overflow (__a, __b, &result);
  return !overflow ? result : __a < 0 ? LONG_MIN : LONG_MAX;
}

/*
** ssub64_imm:
**	subs	x([0-9]+), x0, 67
**	mov	x([0-9]+), -9223372036854775808
**	csel	x0, x\1, x\2, vc
**	ret
*/
int64_t  __attribute__((noipa))
ssub64_imm (int64_t __a)
{
  int64_t result;
  bool overflow = __builtin_sub_overflow (__a, (int64_t) 67, &result);
  return !overflow ? result : __a < 0 ? LONG_MIN : LONG_MAX;
}

/*
** ssub64_imm2:
**	adds	x([0-9]+), x0, #67
**	mov	x([0-9]+), 9223372036854775807
**	csel	x0, x\1, x\2, vc
**	ret
*/
int64_t  __attribute__((noipa))
ssub64_imm2 (int64_t __a)
{
  int64_t result;
  bool overflow = __builtin_sub_overflow (__a, (int64_t) -67, &result);
  return !overflow ? result : __a < 0 ? LONG_MIN : LONG_MAX;
}

int
main (void)
{
  /* Addition:
  SAT_ADD(x, +ve), non-saturating
  SAT_ADD(x, +ve), saturating
  SAT_ADD(x, immediate +ve)
  SAT_ADD(x, immediate -ve)
  SAT_ADD(x, -ve), non-saturating
  SAT_ADD(x, -ve), saturating

  Subtraction:
  SAT_SUB(x, +ve), non-saturating
  SAT_SUB(x, +ve), saturating
  SAT_SUB(x, immediate +ve)
  SAT_SUB(x, immediate -ve)
  SAT_SUB(x, -ve), non-saturating  */

  int32_t a = 4;
  int32_t b = 70;
  int32_t c = 2147483647;
  int32_t d = (int32_t) -2147483648;

  if (sadd32 (a, b) != (a + b))
    __builtin_abort ();
  if (sadd32 (a, c) != c)
    __builtin_abort ();
  if (sadd32_imm (a) != (a + 67))
    __builtin_abort ();
  if (sadd32_imm2 (a) != (a - 67))
    __builtin_abort ();
  if (sadd32 (a, -b) != (a - b))
    __builtin_abort ();
  if (sadd32 (a, d) != (d + 4))
    __builtin_abort ();

  if (ssub32 (a, b) != (a - b))
    __builtin_abort ();
  if (ssub32 (-a, c) != d)
    __builtin_abort ();
  if (ssub32_imm (a) != (a - 67))
    __builtin_abort ();
  if (ssub32_imm2 (a) != (a + 67))
    __builtin_abort ();
  if (ssub32 (a, -b) != (a + b))
    __builtin_abort ();

  int64_t a_64 = a;
  int64_t b_64 = b;
  int64_t c_64 = (int64_t) 9223372036854775807;
  int64_t d_64 = (int64_t) 0x8000000000000000;

  if (sadd64 (a_64, b_64) != (a_64 + b_64))
    __builtin_abort ();
  if (sadd64 (a_64, c_64) != c_64)
    __builtin_abort ();
  if (sadd64_imm (a_64) != (a_64 + 67))
    __builtin_abort ();
  if (sadd64_imm2 (a_64) != (a_64 - 67))
    __builtin_abort ();
  if (sadd64 (a_64, -b_64) != (a_64 - b_64))
    __builtin_abort ();
  if (sadd64 (a_64, d_64) != (d_64 + 4))
    __builtin_abort ();

  if (ssub64 (a_64, b_64) != (a_64 - b_64))
    __builtin_abort ();
  if (ssub64 (-a_64, c_64) != d_64)
    __builtin_abort ();
  if (ssub64_imm (a_64) != (a_64 - 67))
    __builtin_abort ();
  if (ssub64_imm2 (a_64) != (a_64 + 67))
    __builtin_abort ();
  if (ssub64 (a_64, -b_64) != (a_64 + b_64))
    __builtin_abort ();

  return 0;
}