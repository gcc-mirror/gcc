/* Test for octal integer constants.  */

/* Derived from: binary-constants-1.c, by Jakub Jelinek <jakub@redhat.com>.  */
/* Origin: Joerg Wunsch <j.gnu@uriah.heep.sax.de>.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <limits.h>

/* Assertion that constant C is of type T.  */
#define ASSERT_CONST_TYPE(C, T)			\
	do {					\
	  typedef T type;			\
	  typedef type **typepp;		\
	  typedef __typeof__((C)) ctype;	\
	  typedef ctype **ctypepp;		\
	  typepp x = 0;				\
	  ctypepp y = 0;			\
	  x = y;				\
	  y = x;				\
	} while (0)

/* (T *) if E is zero, (void *) otherwise.  */
#define type_if_not(T, E) __typeof__(0 ? (T *)0 : (void *)(E))

/* (T *) if E is nonzero, (void *) otherwise.  */
#define type_if(T, E) type_if_not(T, !(E))

/* Combine pointer types, all but one (void *).  */
#define type_comb2(T1, T2) __typeof__(0 ? (T1)0 : (T2)0)
#define type_comb3(T1, T2, T3) type_comb2(T1, type_comb2(T2, T3))
#define type_comb4(T1, T2, T3, T4)				\
	type_comb2(T1, type_comb2(T2, type_comb2(T3, T4)))
#define type_comb6(T1, T2, T3, T4, T5, T6)				   \
	type_comb2(T1,							   \
		   type_comb2(T2,					   \
			      type_comb2(T3,				   \
					 type_comb2(T4,			   \
						    type_comb2(T5, T6)))))

/* (T1 *) if E1, otherwise (T2 *) if E2.  */
#define first_of2p(T1, E1, T2, E2) type_comb2(type_if(T1, (E1)),	   \
					     type_if(T2, (!(E1) && (E2))))
/* (T1 *) if E1, otherwise (T2 *) if E2, otherwise (T3 *) if E3.  */
#define first_of3p(T1, E1, T2, E2, T3, E3)			\
	type_comb3(type_if(T1, (E1)),				\
		   type_if(T2, (!(E1) && (E2))),		\
		   type_if(T3, (!(E1) && !(E2) && (E3))))
/* (T1 *) if E1, otherwise (T2 *) if E2, otherwise (T3 *) if E3, otherwise
   (T4 *) if E4.  */
#define first_of4p(T1, E1, T2, E2, T3, E3, T4, E4)			\
	type_comb4(type_if(T1, (E1)),					\
		   type_if(T2, (!(E1) && (E2))),			\
		   type_if(T3, (!(E1) && !(E2) && (E3))),		\
		   type_if(T4, (!(E1) && !(E2) && !(E3) && (E4))))
/* (T1 *) if E1, otherwise (T2 *) if E2, otherwise (T3 *) if E3, otherwise
   (T4 *) if E4, otherwise (T5 *) if E5, otherwise (T6 *) if E6.  */
#define first_of6p(T1, E1, T2, E2, T3, E3, T4, E4, T5, E5, T6, E6)	    \
	type_comb6(type_if(T1, (E1)),					    \
		   type_if(T2, (!(E1) && (E2))),			    \
		   type_if(T3, (!(E1) && !(E2) && (E3))),		    \
		   type_if(T4, (!(E1) && !(E2) && !(E3) && (E4))),	    \
		   type_if(T5, (!(E1) && !(E2) && !(E3) && !(E4) && (E5))), \
		   type_if(T6, (!(E1) && !(E2) && !(E3)			    \
				&& !(E4) && !(E5) && (E6))))

/* Likewise, but return the original type rather than a pointer type.  */
#define first_of2(T1, E1, T2, E2)			\
	__typeof__(*((first_of2p(T1, (E1), T2, (E2)))0))
#define first_of3(T1, E1, T2, E2, T3, E3)				\
	__typeof__(*((first_of3p(T1, (E1), T2, (E2), T3, (E3)))0))
#define first_of4(T1, E1, T2, E2, T3, E3, T4, E4)			    \
	__typeof__(*((first_of4p(T1, (E1), T2, (E2), T3, (E3), T4, (E4)))0))
#define first_of6(T1, E1, T2, E2, T3, E3, T4, E4, T5, E5, T6, E6)	\
	__typeof__(*((first_of6p(T1, (E1), T2, (E2), T3, (E3),		\
				 T4, (E4), T5, (E5), T6, (E6)))0))

/* Types of constants according to the C99 rules.  */
#define C99_UNSUF_TYPE(C)					\
	first_of6(int, (C) <= INT_MAX,				\
		  unsigned int, (C) <= UINT_MAX,		\
		  long int, (C) <= LONG_MAX,			\
		  unsigned long int, (C) <= ULONG_MAX,		\
		  long long int, (C) <= LLONG_MAX,		\
		  unsigned long long int, (C) <= ULLONG_MAX)
#define C99_SUFu_TYPE(C)					\
	first_of3(unsigned int, (C) <= UINT_MAX,		\
		  unsigned long int, (C) <= ULONG_MAX,		\
		  unsigned long long int, (C) <= ULLONG_MAX)
#define C99_SUFl_TYPE(C)					\
	first_of4(long int, (C) <= LONG_MAX,			\
		  unsigned long int, (C) <= ULONG_MAX,		\
		  long long int, (C) <= LLONG_MAX,		\
		  unsigned long long int, (C) <= ULLONG_MAX)
#define C99_SUFul_TYPE(C)					\
	first_of2(unsigned long int, (C) <= ULONG_MAX,		\
		  unsigned long long int, (C) <= ULLONG_MAX)
#define C99_SUFll_TYPE(C)					\
	first_of2(long long int, (C) <= LLONG_MAX,		\
		  unsigned long long int, (C) <= ULLONG_MAX)

/* Checks that constants have correct type.  */
#define CHECK_UNSUF_TYPE(C)				\
	ASSERT_CONST_TYPE((C), C99_UNSUF_TYPE((C)))
#define CHECK_SUFu_TYPE(C) ASSERT_CONST_TYPE((C), C99_SUFu_TYPE((C)))
#define CHECK_SUFl_TYPE(C)				\
	ASSERT_CONST_TYPE((C), C99_SUFl_TYPE((C)))
#define CHECK_SUFul_TYPE(C) ASSERT_CONST_TYPE((C), C99_SUFul_TYPE((C)))
#define CHECK_SUFll_TYPE(C)				\
	ASSERT_CONST_TYPE((C), C99_SUFll_TYPE((C)))
#define CHECK_SUFull_TYPE(C) ASSERT_CONST_TYPE((C), unsigned long long int)

/* Check an octal or hexadecimal value, with all suffixes.  */
#define CHECK_CONST(C)                  	\
	CHECK_UNSUF_TYPE(C);                    \
	CHECK_SUFu_TYPE(C##u);			\
	CHECK_SUFu_TYPE(C##U);			\
	CHECK_SUFl_TYPE(C##l);                  \
	CHECK_SUFl_TYPE(C##L);                  \
	CHECK_SUFul_TYPE(C##ul);		\
	CHECK_SUFul_TYPE(C##uL);		\
	CHECK_SUFul_TYPE(C##Ul);		\
	CHECK_SUFul_TYPE(C##UL);		\
	CHECK_SUFll_TYPE(C##ll);		\
	CHECK_SUFll_TYPE(C##LL);		\
	CHECK_SUFull_TYPE(C##ull);		\
	CHECK_SUFull_TYPE(C##uLL);		\
	CHECK_SUFull_TYPE(C##Ull);		\
	CHECK_SUFull_TYPE(C##ULL);

#define CHECK_OCT_CONST(C)			\
	CHECK_CONST(0o##C);                     \
	CHECK_CONST(0O##C);

/* True iff "long long" is at least B bits.  This presumes that (B-2)/3 is at
   most 63.  */
#define LLONG_AT_LEAST(B)			\
	(LLONG_MAX >> ((B)-2)/3 >> ((B)-2)/3	\
	 >> ((B)-2 - ((B)-2)/3 - ((B)-2)/3))

#define LLONG_HAS_BITS(B) (LLONG_AT_LEAST((B)) && !LLONG_AT_LEAST((B) + 1))

#define FOO 0o1307
#if !FOO
# error "preprocessor does not accept octal constants"
#endif

void
foo (void)
{
  /* Check all 2^n and 2^n - 1 up to 2^72 - 1.  */
  CHECK_OCT_CONST(1);
  CHECK_OCT_CONST(2);
  CHECK_OCT_CONST(3);
  CHECK_OCT_CONST(4);
  CHECK_OCT_CONST(7);
  CHECK_OCT_CONST(10);
  CHECK_OCT_CONST(17);
  CHECK_OCT_CONST(20);
  CHECK_OCT_CONST(37);
  CHECK_OCT_CONST(40);
  CHECK_OCT_CONST(77);
  CHECK_OCT_CONST(100);
  CHECK_OCT_CONST(177);
  CHECK_OCT_CONST(200);
  CHECK_OCT_CONST(377);
  CHECK_OCT_CONST(400);
  CHECK_OCT_CONST(777);
  CHECK_OCT_CONST(1000);
  CHECK_OCT_CONST(1777);
  CHECK_OCT_CONST(2000);
  CHECK_OCT_CONST(3777);
  CHECK_OCT_CONST(4000);
  CHECK_OCT_CONST(7777);
  CHECK_OCT_CONST(10000);
  CHECK_OCT_CONST(17777);
  CHECK_OCT_CONST(20000);
  CHECK_OCT_CONST(37777);
  CHECK_OCT_CONST(40000);
  CHECK_OCT_CONST(77777);
  CHECK_OCT_CONST(100000);
  CHECK_OCT_CONST(177777);
  CHECK_OCT_CONST(200000);
  CHECK_OCT_CONST(377777);
  CHECK_OCT_CONST(400000);
  CHECK_OCT_CONST(777777);
  CHECK_OCT_CONST(1000000);
  CHECK_OCT_CONST(1777777);
  CHECK_OCT_CONST(2000000);
  CHECK_OCT_CONST(3777777);
  CHECK_OCT_CONST(4000000);
  CHECK_OCT_CONST(7777777);
  CHECK_OCT_CONST(10000000);
  CHECK_OCT_CONST(17777777);
  CHECK_OCT_CONST(20000000);
  CHECK_OCT_CONST(37777777);
  CHECK_OCT_CONST(40000000);
  CHECK_OCT_CONST(77777777);
  CHECK_OCT_CONST(100000000);
  CHECK_OCT_CONST(177777777);
  CHECK_OCT_CONST(200000000);
  CHECK_OCT_CONST(377777777);
  CHECK_OCT_CONST(400000000);
  CHECK_OCT_CONST(777777777);
  CHECK_OCT_CONST(1000000000);
  CHECK_OCT_CONST(1777777777);
  CHECK_OCT_CONST(2000000000);
  CHECK_OCT_CONST(3777777777);
  CHECK_OCT_CONST(4000000000);
  CHECK_OCT_CONST(7777777777);
  CHECK_OCT_CONST(10000000000);
  CHECK_OCT_CONST(17777777777);
  CHECK_OCT_CONST(20000000000);
  CHECK_OCT_CONST(37777777777);
  CHECK_OCT_CONST(40000000000);
  CHECK_OCT_CONST(77777777777);
  CHECK_OCT_CONST(100000000000);
  CHECK_OCT_CONST(177777777777);
  CHECK_OCT_CONST(200000000000);
  CHECK_OCT_CONST(377777777777);
  CHECK_OCT_CONST(400000000000);
  CHECK_OCT_CONST(777777777777);
  CHECK_OCT_CONST(1000000000000);
  CHECK_OCT_CONST(1777777777777);
  CHECK_OCT_CONST(2000000000000);
  CHECK_OCT_CONST(3777777777777);
  CHECK_OCT_CONST(4000000000000);
  CHECK_OCT_CONST(7777777777777);
  CHECK_OCT_CONST(10000000000000);
  CHECK_OCT_CONST(17777777777777);
  CHECK_OCT_CONST(20000000000000);
  CHECK_OCT_CONST(37777777777777);
  CHECK_OCT_CONST(40000000000000);
  CHECK_OCT_CONST(77777777777777);
  CHECK_OCT_CONST(100000000000000);
  CHECK_OCT_CONST(177777777777777);
  CHECK_OCT_CONST(200000000000000);
  CHECK_OCT_CONST(377777777777777);
  CHECK_OCT_CONST(400000000000000);
  CHECK_OCT_CONST(777777777777777);
  CHECK_OCT_CONST(1000000000000000);
  CHECK_OCT_CONST(1777777777777777);
  CHECK_OCT_CONST(2000000000000000);
  CHECK_OCT_CONST(3777777777777777);
  CHECK_OCT_CONST(4000000000000000);
  CHECK_OCT_CONST(7777777777777777);
  CHECK_OCT_CONST(10000000000000000);
  CHECK_OCT_CONST(17777777777777777);
  CHECK_OCT_CONST(20000000000000000);
  CHECK_OCT_CONST(37777777777777777);
  CHECK_OCT_CONST(40000000000000000);
  CHECK_OCT_CONST(77777777777777777);
  CHECK_OCT_CONST(100000000000000000);
  CHECK_OCT_CONST(177777777777777777);
  CHECK_OCT_CONST(200000000000000000);
  CHECK_OCT_CONST(377777777777777777);
  CHECK_OCT_CONST(400000000000000000);
  CHECK_OCT_CONST(777777777777777777);
  CHECK_OCT_CONST(1000000000000000000);
  CHECK_OCT_CONST(1777777777777777777);
  CHECK_OCT_CONST(2000000000000000000);
  CHECK_OCT_CONST(3777777777777777777);
  CHECK_OCT_CONST(4000000000000000000);
  CHECK_OCT_CONST(7777777777777777777);
  CHECK_OCT_CONST(10000000000000000000);
  CHECK_OCT_CONST(17777777777777777777);
  CHECK_OCT_CONST(20000000000000000000);
  CHECK_OCT_CONST(37777777777777777777);
  CHECK_OCT_CONST(40000000000000000000);
  CHECK_OCT_CONST(77777777777777777777);
  CHECK_OCT_CONST(100000000000000000000);
  CHECK_OCT_CONST(177777777777777777777);
  CHECK_OCT_CONST(200000000000000000000);
  CHECK_OCT_CONST(377777777777777777777);
  CHECK_OCT_CONST(400000000000000000000);
  CHECK_OCT_CONST(777777777777777777777);
  CHECK_OCT_CONST(1000000000000000000000);
  CHECK_OCT_CONST(1777777777777777777777);
#if LLONG_AT_LEAST(65)
  CHECK_OCT_CONST(2000000000000000000000);
  CHECK_OCT_CONST(3777777777777777777777);
#endif
#if LLONG_AT_LEAST(66)
  CHECK_OCT_CONST(4000000000000000000000);
  CHECK_OCT_CONST(7777777777777777777777);
#endif
#if LLONG_AT_LEAST(67)
  CHECK_OCT_CONST(10000000000000000000000);
  CHECK_OCT_CONST(17777777777777777777777);
#endif
#if LLONG_AT_LEAST(68)
  CHECK_OCT_CONST(20000000000000000000000);
  CHECK_OCT_CONST(37777777777777777777777);
#endif
#if LLONG_AT_LEAST(69)
  CHECK_OCT_CONST(40000000000000000000000);
  CHECK_OCT_CONST(77777777777777777777777);
#endif
#if LLONG_AT_LEAST(70)
  CHECK_OCT_CONST(100000000000000000000000);
  CHECK_OCT_CONST(177777777777777777777777);
#endif
#if LLONG_AT_LEAST(71)
  CHECK_OCT_CONST(200000000000000000000000);
  CHECK_OCT_CONST(377777777777777777777777);
#endif
#if LLONG_AT_LEAST(72)
  CHECK_OCT_CONST(400000000000000000000000);
  CHECK_OCT_CONST(777777777777777777777777);
#endif
}
