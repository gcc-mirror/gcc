/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* Test the full range of preprocessor operator precedence.  Each
   operator is tested with one of immediately higher precedence to
   verify it is of strictly lower precedence.  To avoid complications,
   each test uses just those two operators.  Occasionally this assumes
   correct operation of if-then-else, so the first tests verify this.  */

/* { dg-do preprocess } */

/* Ensure correct functioning of if-then-else.  */
#if 1
#else
#error #else block evaluated for true conditional
#endif

#if 0
#error #if block evaluated for false conditional
#else
#endif

/* , not higher than ?.  This is not a syntax error if it is.  */
#if 1 ? 0, 1: 1	/* { dg-error "syntax" "? higher precedence than ," } */
#error
#endif

/* : strictly higher than ?.  This would give a syntax error otherwise.  */
#if 0 ? 0 : 1 ? 1 : 1
#endif

/* || strictly higher than ?:. */
#if 1 ? 0: 0 || 1
#error operator ?: has higher precedence than operator ||
#endif

/* && strictly higher than ||.  */
#if 1 || 0 && 0
#else
#error operator || has higher precedence than operator &&
#endif

/* | strictly higher than &&.  */
#if 0 && 0 | 1
#error operator && has higher precedence than operator |
#endif

/* ^ strictly higher than |.  */
#if 1 | 0 ^ 1
#else
#error operator | has higher precedence than operator ^
#endif

/* & strictly higher than ^.  */
#if 1 ^ 0 & 0
#else
#error operator ^ has higher precedence than operator &
#endif

/* == (!=) strictly higher than &.  */
#if 0 & 0 == 0
#error operator & has higher precedence than operator ==
#endif

/* < (>, <=, >=) strictly higher than == (!=).  */

#if 0 == 0 < 0
#else
#error operator == has higher precedence than operator <
#endif

/* << (>>) strictly higher than < (>, <=, >=).  */
#if 1 < 1 << 1
#else
#error operator < has higher precedence than operator <<
#endif

/* Binary + (-) strictly higher than << (>>).  */
#if 0 << 0 + 1
#error operator << has higher precedence than binary +
#endif

/* Binary * (/, %) strictly higher than binary + (-).  */
#if 1 + 0 * 0
#else
#error binary + has higher precedence than binary *
#endif

/* Unary operators (!, ~, -, +) strictly higher than binary * (/, %).
   Equality is hard to detect because of right-associativity.  */
#if ~1 * 0
#error binary * has higher precedence than operator ~
#endif

/* () > Unary.  Unfortunately this requires an additional operator.  */
#if -(1 - 1)
#error unary - has higher precedence than operator ()
#endif
