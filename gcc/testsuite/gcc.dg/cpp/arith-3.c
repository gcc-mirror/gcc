/* Preprocessor arithmetic semantic tests.  */

/* Copyright (C) 2002 Free Software Foundation, Inc.  */
/* Source: Neil Booth, 26 May 2002.  */

/* The file tests overflow warnings for, and values of, preprocessor
   arithmetic that are dependent on target precision.

   Please keep changes to arith-2.c and arith-3.c in sync.  */

/* { dg-do preprocess } */
/* { dg-options "-std=c99 -fshow-column" } */

#include <limits.h>

#define APPEND2(NUM, SUFF) NUM ## SUFF
#define APPEND(NUM, SUFF) APPEND2(NUM, SUFF)

#define TARGET_UTYPE_MAX  ULLONG_MAX

/* The tests in this file depend only on the macros defined in this
   #if block.  Note that it is no good calculating these values, as
   the intent is to test both the preprocessor's number parser and
   arithmetic.  */
#if TARGET_UTYPE_MAX == 65535ULL

#  define TARG_PRECISION 16
#  define MAX_INT  32767
#  define MAX_UINT 65535

#  define TARG_MAX_HEX 0x7fff
#  define TARG_MAX_OCT 077777
#  define TARG_MAX_PLUS_1 32768L
#  define TARG_MAX_PLUS_1_U 32768UL
#  define TARG_MAX_PLUS_1_HEX 0x8000
#  define TARG_MAX_PLUS_1_OCT 0100000
#  define UTARG_MAX_HEX 0xffff
#  define UTARG_MAX_OCT 0177777
#  define UTARG_MAX_PLUS_1 65536L
#  define UTARG_MAX_PLUS_1_HEX 0x10000
#  define UTARG_MAX_PLUS_1_OCT 0200000

#  define TARG_LOWPART_PLUS_1 256L
#  define TARG_LOWPART_PLUS_1_U 256UL

  /* Division and modulo; anything that uses the high half in both
     dividend and divisor.  */
#  define LONG_UDIVISION 61234UL / 260L
#  define LONG_UDIVISION_ANSWER 235
#  define LONG_SDIVISION -15000L / 299L
#  define LONG_SDIVISION_ANSWER -50
#  define LONG_UMODULO 61234UL % 260L
#  define LONG_UMODULO_ANSWER 134
#  define LONG_SMODULO -15000L % 299L
#  define LONG_SMODULO_ANSWER -50

#elif TARGET_UTYPE_MAX == 4294967295ULL

#  define TARG_PRECISION 32
#  define MAX_INT  2147483647
#  define MAX_UINT 4294967295

#  define TARG_MAX_HEX 0x7fffffff
#  define TARG_MAX_OCT 017777777777
#  define TARG_MAX_PLUS_1 2147483648L
#  define TARG_MAX_PLUS_1_U 2147483648UL
#  define TARG_MAX_PLUS_1_HEX 0x80000000
#  define TARG_MAX_PLUS_1_OCT 020000000000
#  define UTARG_MAX_HEX 0xffffffff
#  define UTARG_MAX_OCT 037777777777
#  define UTARG_MAX_PLUS_1 4294967296L
#  define UTARG_MAX_PLUS_1_HEX 0x100000000
#  define UTARG_MAX_PLUS_1_OCT 040000000000

#  define TARG_LOWPART_PLUS_1 65536
#  define TARG_LOWPART_PLUS_1_U 65536UL

  /* Division and modulo; anything that uses the high half in both
     dividend and divisor.  */
#  define LONG_UDIVISION 268335456UL / 70000L
#  define LONG_UDIVISION_ANSWER 3833
#  define LONG_SDIVISION -368335456L / 123456L
#  define LONG_SDIVISION_ANSWER -2983
#  define LONG_UMODULO 268335456UL % 70000L
#  define LONG_UMODULO_ANSWER 25456
#  define LONG_SMODULO -368335456L % 123456L
#  define LONG_SMODULO_ANSWER -66208

#elif TARGET_UTYPE_MAX == 18446744073709551615ULL

#  define TARG_PRECISION 64
#  define MAX_INT  9223372036854775807
#  define MAX_UINT 18446744073709551615

#  define TARG_MAX_HEX 0x7fffffffffffffff
#  define TARG_MAX_OCT 0777777777777777777777
#  define TARG_MAX_PLUS_1 9223372036854775808L
#  define TARG_MAX_PLUS_1_U 9223372036854775808UL
#  define TARG_MAX_PLUS_1_HEX 0x8000000000000000
#  define TARG_MAX_PLUS_1_OCT 01000000000000000000000
#  define UTARG_MAX_HEX 0xffffffffffffffff
#  define UTARG_MAX_OCT 01777777777777777777777
#  define UTARG_MAX_PLUS_1 18446744073709551616L
#  define UTARG_MAX_PLUS_1_HEX 0x10000000000000000
#  define UTARG_MAX_PLUS_1_OCT 02000000000000000000000

#  define TARG_LOWPART_PLUS_1 4294967296
#  define TARG_LOWPART_PLUS_1_U 4294967296U

  /* Division and modulo; anything that uses the high half in both
     dividend and divisor.  */
#  define LONG_UDIVISION 235184372088832UL / 17279869184L
#  define LONG_UDIVISION_ANSWER 13610
#  define LONG_SDIVISION -234582345927345L / 12345678901L
#  define LONG_SDIVISION_ANSWER -19001
#  define LONG_UMODULO 235184372088832UL % 17279869184L
#  define LONG_UMODULO_ANSWER 5352494592L
#  define LONG_SMODULO -234582345927345L % 12345678901L
#  define LONG_SMODULO_ANSWER -2101129444L

#else

#  error Please extend the macros here so that this file tests your target

#endif

/* Create more macros based on the above.  */
#define TARG_PART_BITS (TARG_PRECISION / 2)
#define TARG_MIN (-TARG_MAX - 1)
#define TARG_MAX APPEND (MAX_INT, L)
#define TARG_MAX_U APPEND (MAX_INT, UL)
#define UTARG_MAX APPEND (MAX_UINT, L)
#define UTARG_MAX_U APPEND (MAX_UINT, UL)

/* And now the tests.  */

#if TARG_MAX			/* { dg-bogus "so large" }  */
#endif
#if TARG_MAX_PLUS_1_HEX		/* { dg-bogus "so large" }  */
#endif
#if TARG_MAX_PLUS_1_OCT		/* { dg-bogus "so large" }  */
#endif

#if UTARG_MAX			/* { dg-warning "so large" }  */
#endif
#if UTARG_MAX_PLUS_1		/* { dg-warning "too large" }  */
#endif
#if UTARG_MAX_PLUS_1_HEX	/* { dg-warning "too large" }  */
#endif
#if UTARG_MAX_HEX		/* { dg-bogus "too large" }  */
#endif
#if UTARG_MAX_PLUS_1_OCT	/* { dg-warning "too large" }  */
#endif
#if UTARG_MAX_OCT		/* { dg-bogus "too large" }  */
#endif

#if TARG_MAX < 0 || TARG_MAX_PLUS_1 < 0	/* { dg-warning "so large" } */
# error		/* { dg-bogus "error" }  */
#endif

#if UTARG_MAX_HEX < 0 || TARG_MAX_HEX < 0
# error		/* { dg-bogus "error" }  */
#endif

#if UTARG_MAX_OCT < 0 || TARG_MAX_OCT < 0
# error		/* { dg-bogus "error" }  */
#endif

#if -1 != UTARG_MAX_U
# error		/* { dg-bogus "error" }  */
#endif




/* Test each operator correctly warns of overflow conditions, and
   gives the right answer.  */

/* Binary +.  */
#if TARG_MAX + 1 != TARG_MIN	/* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -TARG_MAX + -2 != TARG_MAX	/* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -TARG_MAX + -1 != TARG_MIN	/* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MAX_U + 1 != TARG_MIN	/* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -TARG_MAX_U + -2 != TARG_MAX /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif




/* Binary -.  */
#if TARG_MAX - -1 != TARG_MIN	/* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -TARG_MAX - 2 != TARG_MAX	/* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -TARG_MAX - 1 != TARG_MIN	/* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MAX_U - -1 != TARG_MIN	/* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -TARG_MAX_U - 2 != TARG_MAX /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif





/* Binary *.  */
#if TARG_LOWPART_PLUS_1 * (TARG_LOWPART_PLUS_1 >> 1) != TARG_MIN /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if (TARG_LOWPART_PLUS_1 >> 1) * TARG_LOWPART_PLUS_1 != TARG_MIN /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if (TARG_LOWPART_PLUS_1 << 1) * (TARG_LOWPART_PLUS_1 + 1) != (TARG_LOWPART_PLUS_1 << 1) /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MAX * 1 != TARG_MAX	/* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if (TARG_MAX >> 1) * 2	!= TARG_MAX - 1 /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if (TARG_LOWPART_PLUS_1_U + 61) * (TARG_LOWPART_PLUS_1 << 1) != 61 * (TARG_LOWPART_PLUS_1 << 1) /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if (TARG_LOWPART_PLUS_1 >> 1) * TARG_LOWPART_PLUS_1_U != TARG_MIN /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if 1 * TARG_MIN != TARG_MIN	/* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif




/* Binary /.  */
#if TARG_MIN / -1 != TARG_MIN	/* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MIN / 1 != TARG_MIN	/* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -TARG_MAX_PLUS_1_U / -1 != 0 /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -5 / (2 - 2) /* { dg-error "13:division by zero" } */
#endif

#if LONG_UDIVISION != LONG_UDIVISION_ANSWER
# error		/* { dg-bogus "error" }  */
#endif

#if LONG_SDIVISION != LONG_SDIVISION_ANSWER
# error		/* { dg-bogus "error" }  */
#endif

/* Binary %.  Cannot overflow.  */
#if -5 % (2 - 2) /* { dg-error "13:division by zero" } */
#endif

#if TARG_MIN % 1 /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if LONG_UMODULO != LONG_UMODULO_ANSWER
# error		/* { dg-bogus "error" }  */
#endif

#if LONG_SMODULO != LONG_SMODULO_ANSWER
# error		/* { dg-bogus "error" }  */
#endif

#if 234 % -1U != 234
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MIN % -1U != TARG_MIN
# error		/* { dg-bogus "error" }  */
#endif

/* Binary << and Binary >>, the latter cannot overflow.  */
#if -1 >> 3 != -1     /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MAX >> 3 != TARG_MAX / 8     /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if 0 << 256 != 0  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if 1 << 256 != 0 /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if 1U << 256 != 0  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MAX << 1 != -2  /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MAX_U << 1 != -2  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_LOWPART_PLUS_1 << TARG_PART_BITS != 0  /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_LOWPART_PLUS_1 << (TARG_PART_BITS - 1) != TARG_MIN  /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_LOWPART_PLUS_1_U << (TARG_PART_BITS - 1) != TARG_MIN  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_LOWPART_PLUS_1 << (TARG_PART_BITS - 2) != (TARG_MAX_PLUS_1_U >> 1)  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

/* Test how the sign bit is handled.  */
#if (TARG_MIN << 1) != 0    /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if (TARG_MAX_PLUS_1_U << 1) != 0    /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if (TARG_MIN >> 1) != 3U << (TARG_PRECISION - 2)    /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if (TARG_MAX_PLUS_1_U >> 1) != 1 << (TARG_PRECISION - 2)    /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif



/* Unary -.  It can overflow in just one case.  */
#if -TARG_MIN != TARG_MIN  /* { dg-warning "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if - -TARG_MAX != TARG_MAX   /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif




/* Unary +, ~, and !.  They cannot overflow.  */
#if +TARG_MAX != TARG_MAX  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if !TARG_MAX + !TARG_MIN != 0   /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if ~TARG_MAX , ~TARG_MIN != TARG_MAX  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif




/* Bitwise &, ^, |.  They cannot overflow.  */
#if (TARG_MAX & -1), (TARG_MIN & -1) != TARG_MIN  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MAX | -1, (TARG_MIN | -1) != -1  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if TARG_MAX ^ -1, (TARG_MIN ^ -1) != TARG_MAX  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif




/* Comparison operators.  They cannot overflow.  */
#if -1 <= TARG_MAX, (TARG_MIN <= 1) != 1  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -1 >= TARG_MAX, (TARG_MIN >= 1) != 0  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -1 < TARG_MAX, (TARG_MIN < 1) != 1  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -1 > TARG_MAX, (TARG_MIN > 1) != 0  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif




/* Comma and ? : operators.  They cannot overflow.  */
#if -1, TARG_MAX, TARG_MIN != TARG_MIN  /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif

#if -1 ? TARG_MAX: TARG_MAX, 0 ? 1: TARG_MIN != TARG_MIN /* { dg-bogus "overflow" } */
# error		/* { dg-bogus "error" }  */
#endif
