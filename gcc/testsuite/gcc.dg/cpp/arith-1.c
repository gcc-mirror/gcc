/* Preprocessor arithmetic semantic tests.  */

/* Copyright (C) 2002 Free Software Foundation, Inc.  */
/* Source: Neil Booth, 25 May 2002.  */

/* The file tests all aspects of preprocessor arithmetic that are
   independent of target precision.  */

/* { dg-do preprocess } */
/* { dg-options -fno-show-column } */

/* Test || operator and its short circuiting.  */
#if 0 || 0
# error		/* { dg-bogus "error" }  */
#endif

#if 5 || 0
#else
# error		/* { dg-bogus "error" }  */
#endif

#if 0 || 1
#else
# error		/* { dg-bogus "error" }  */
#endif

#if 1 || 4
#else
# error		/* { dg-bogus "error" }  */
#endif

#if 1 || (8 / 0) /* { dg-bogus "division by zero" }  */
#else
# error		/* { dg-bogus "error" }  */
#endif

#if 1 || (1 << 256) /* { dg-bogus "overflow" }  */
#endif

/* Test && operator and its short circuiting.  */
#if (0 && 0) || (0 && 1) || (1 && 0)
# error		/* { dg-bogus "error" }  */
#endif

#if 1 && 2
#else
# error		/* { dg-bogus "error" }  */
#endif

#if 0 && (8 / 0)/* { dg-bogus "division by zero" }  */
# error		/* { dg-bogus "error" }  */
#endif

#if 0 && (1 << 256) /* { dg-bogus "overflow" }  */
#endif

/* Test == and != operators, and their signedness.  */
#if 1 == 0 || 0 == 1 || 20 != 0x014 || 142 != 0216
# error		/* { dg-bogus "error" }  */
#endif

#if (1 == 1) - 2 > 0 || (1U != 1U) - 2 > 0
# error		/* { dg-bogus "error" }  */
#endif

/* Test ? : operator, its short circuiting, and its signedness.  */
#if (1 ? 3: 5) != 3 || (0 ? 3: 5) != 5
# error		/* { dg-bogus "error" }  */
#endif

#if 1 ? 0: 1 / 0 /* { dg-bogus "division by zero" }  */
# error		/* { dg-bogus "error" }  */
#endif

#if 0 ? 1 / 0: 0 /* { dg-bogus "division by zero" }  */
# error		/* { dg-bogus "error" }  */
#endif

#if 0 ? (1 << 256): 0 /* { dg-bogus "overflow" }  */
#endif

#if 1 ? 0: (1 << 256) /* { dg-bogus "overflow" }  */
#endif

/* Test unary + and its signedness.  */

#if 23 != +23 || 23 != + +23
# error		/* { dg-bogus "error" }  */
#endif

#if (+1 - 2) > 0 || (+1U - 2) < 0
# error		/* { dg-bogus "error" }  */
#endif

/* Test unary - and its signedness.  */

#if -1 + 1 != 0
# error		/* { dg-bogus "error" }  */
#endif

#if -1 >= 0 || -1U <= 0
# error		/* { dg-bogus "error" }  */
#endif

/* Test unary ! and its signedness.  */
#if !5 != 0 || !1 != 0 || !0 != 1
# error		/* { dg-bogus "error" }  */
#endif

#if !5 - 1 > 0 || !5U - 1 > 0
# error		/* { dg-bogus "error" }  */
#endif

/* Test unary ~ and its signedness.  */
#if ~0 != -1 || ~~5 != 5 || ~-2 != 1
# error		/* { dg-bogus "error" }  */
#endif

#if ~5 > 0 || ~5U < 0
# error		/* { dg-bogus "error" }  */
#endif

/* Test comparison operators and their signedness.  */
#if 1 >= 1 && 2 >= 1 && -1 >= -1 && -1 >= -2 && 1 >= -1 && 1 >= -2 \
    && !(-2 >= -1) && !(2 >= 3) && -1U >= 2 && !(-1 >= 1)
#else
# error		/* { dg-bogus "error" }  */
#endif

#if ((1 > 0) - 2) > 0 || ((1U > 0) - 2) > 0
# error		/* { dg-bogus "error" }  */
#endif

#if !(1 > 1) && 2 > 1 && !(-1 > -1) && -1 > -2 && 1 > -1 && 1 > -2 \
    && !(-2 > -1) && !(2 > 3) && -1U > 2 && !(-1 > 1)
#else
# error		/* { dg-bogus "error" }  */
#endif

#if ((1 >= 0) - 2) > 0 || ((1U >= 0) - 2) > 0
# error		/* { dg-bogus "error" }  */
#endif

#if 1 <= 1 && !(2 <= 1) && -1 <= -1 && !(-1 <= -2) && !(1 <= -1) && !(1 <= -2) \
    && -2 <= -1 && 2 <= 3 && !(-1U <= 2) && -1 <= 1
#else
# error		/* { dg-bogus "error" }  */
#endif

#if ((1 <= 0) - 2) > 0 || ((1U <= 0) - 2) > 0
# error		/* { dg-bogus "error" }  */
#endif

#if !(1 < 1) && !(2 < 1) && !(-1 < -1) && !(-1 < -2) && !(1 < -1) && !(1 < -2) \
    && -2 < -1 && 2 < 3 && !(-1U < 2) && -1 < 1
#else
# error		/* { dg-bogus "error" }  */
#endif

#if ((1 < 0) - 2) > 0 || ((1U < 0) - 2) > 0
# error		/* { dg-bogus "error" }  */
#endif

/* Test bitwise operators and their signedness.  */
#if (3 & 7) != 3 || (-1 & 34) != 34
# error		/* { dg-bogus "error" }  */
#endif

#if (3 & 7) - 20 > 0 || (3 & 7U) - 20 < 0
# error		/* { dg-bogus "error" }  */
#endif

#if (3 | 5) != 7 || (-1 | 34) != -1
# error		/* { dg-bogus "error" }  */
#endif

#if (3 | 7) - 20 > 0 || (3 | 7U) - 20 < 0
# error		/* { dg-bogus "error" }  */
#endif

#if (7 ^ 5) != 2 || (-1 ^ 34) != ~34
# error		/* { dg-bogus "error" }  */
#endif

#if (3 ^ 7) - 20 > 0 || (3 ^ 7U) - 20 < 0
# error		/* { dg-bogus "error" }  */
#endif

/* Test shifts and their signedness.  */
#if 3 << 2 != 12 || 3 << -2 != 0 || -1 << 1 != -2
# error		/* { dg-bogus "error" }  */
#endif

#if 5 >> 1 != 2 || 5 >> -2 != 20 || -5 >> 1 != -3
# error		/* { dg-bogus "error" }  */
#endif

#if (5 >> 2) - 2 >= 0 || (5U >> 2) - 2 <= 0
# error		/* { dg-bogus "error" }  */
#endif

#if (5 << 1) - 20 >= 0 || (5U << 1) - 20 <= 0
# error		/* { dg-bogus "error" }  */
#endif

#if 0
/* Test min / max and their signedness.  */
#if (3 >? 2) != 3 || (-3 >? -2) != -2
# error		/* { dg-bogus "error" }  */
#endif

#if (3 <? 2) != 2 || (-3 <? -2) != -3
# error		/* { dg-bogus "error" }  */
#endif

#if (3 >? 2) - 4 >= 0 || (3 >? 2U) - 4 <= 0
# error		/* { dg-bogus "error" }  */
#endif

#if (3 <? 2) - 4 >= 0 || (3 <? 2U) - 4 <= 0
# error		/* { dg-bogus "error" }  */
#endif
#endif

/* Test *, / and % and their signedness.  */
#if 3 * 2 != 6 || 3 * -2 != -6 || -2 * 3 != -6 || -2 * -3 != 6
# error		/* { dg-bogus "error" }  */
#endif

#if 3 * 2 - 7 >= 0 || 3 * 2U - 7 < 0
# error		/* { dg-bogus "error" }  */
#endif

#if 5 / 2 != 2 || -325 / 50 != -6 || 53 / -4 != -13 || -55 / -12 != 4
# error		/* { dg-bogus "error" }  */
#endif

#if 3 / 2 - 7 >= 0 || 3 / 2U - 7 < 0
# error		/* { dg-bogus "error" }  */
#endif

#if 5 % 2 != 1 || -325 % 50 != -25 || 53 % -4 != 1 || -55 % -12 != -7
# error		/* { dg-bogus "error" }  */
#endif

#if 3 % 2 - 7 >= 0 || 3U % 2 - 7 < 0
# error		/* { dg-bogus "error" }  */
#endif

/* Test , and its signedness.  */
#if (1, 2) != 2 || (2, 1) != 1
# error		/* { dg-bogus "error" }  */
#endif

#if (1, 2) - 3 >= 0 || (1, 2U) - 3 <= 0 || (1U, 2) - 3 >= 0
# error		/* { dg-bogus "error" }  */
#endif
