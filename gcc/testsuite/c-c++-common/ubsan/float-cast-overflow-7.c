/* { dg-do run } */
/* { dg-skip-if "" { ! run_expensive_tests } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=float-cast-overflow -fno-sanitize-recover=float-cast-overflow" } */
/* FIXME: When _DecimalXX <-> {signed, unsigned} __int128 conversions are
   supported, -DBROKEN_DECIMAL_INT128 can be removed.  */
/* { dg-additional-options "-DUSE_DFP -DBROKEN_DECIMAL_INT128" { target dfp } } */

#define USE_FLT_DBL_LDBL
#ifdef __SIZEOF_INT128__
#define USE_INT128
#endif
#ifdef __SIZEOF_FLOAT80__
#define USE_FLOAT80
#endif
#ifdef __SIZEOF_FLOAT128__
#define USE_FLOAT128
#endif

#include "float-cast-overflow-7.h"

#define TEST(type1, type2) \
  if (cvt_##type1##_##type2 (-0.5f) != 0) abort ();		\
  if (cvt_##type1##_##type2 (0.5f) != 0) abort ();		\
  if (cvt_##type1##_##type2 (-0.75f) != 0) abort ();		\
  if (cvt_##type1##_##type2 (0.75f) != 0) abort ();		\
  if (type1##_MIN)						\
    {								\
      /* For RADIX 2 type1##_MIN should be always */		\
      /* exactly representable in type2.  */			\
      if (type2##_RADIX == 2					\
	  || type1##_MAX <= type2##_MAX)			\
	{							\
	  if (cvt_##type1##_##type2 (type1##_MIN)		\
	      != type1##_MIN) abort ();				\
	  volatile type2 tem = ((type2) -0.75f) + type1##_MIN;	\
	  volatile type2 tem2 = ((type2) -1.0f) + type1##_MIN;	\
	  if (tem != tem2					\
	      && cvt_##type1##_##type2 ((type2) -0.75f		\
					+ type1##_MIN)		\
		 != type1##_MIN) abort ();			\
	}							\
      else							\
	{							\
	  type2 min = type1##_MIN;				\
	  /* tem could be below minimum here due to */		\
	  /* rounding.  */					\
	  MAXT add = 1;						\
	  while (add)						\
	    {							\
	      volatile type2 tem = type1##_MIN + (type1) add;	\
	      if (tem != min)					\
		break;						\
	      MAXT newadd = add * type2##_RADIX;		\
	      if (newadd < add || newadd > type1##_MAX)		\
		add = 0;					\
	      else						\
		add = newadd;					\
	    }							\
	  if (add)						\
	    {							\
	      MAXT newadd					\
		= (-(type1##_MIN + (type1) add)) % add;		\
	      volatile type2 tem = type1##_MIN + (type1) newadd;\
	      volatile type2 tem2 = type1##_MIN + (type1) add;	\
	      if (tem == tem2)					\
		add = newadd;					\
	      else						\
		{						\
		  newadd += add;				\
		  if (newadd < add || newadd > type1##_MAX)	\
		    add = 0;					\
		  else						\
		    {						\
		      tem = type1##_MIN + (type1) newadd;	\
		      if (tem == tem2)				\
			add = newadd;				\
		      else					\
			add = 0;				\
		    }						\
		}						\
	    }							\
	  if (add						\
	      && cvt_##type1##_##type2 (type1##_MIN		\
					+ (type1) add)		\
		 != type1##_MIN + (type1) add) abort ();	\
	}							\
    }								\
  if (type1##_MAX <= type2##_MAX)				\
    {								\
      if (cvt_##type1##_##type2 (type1##_MAX) != type1##_MAX)	\
	abort ();						\
      volatile type2 tem = ((type2) 0.75f) + type1##_MAX;	\
      volatile type2 tem2 = ((type2) 1.0f) + type1##_MAX;	\
      if (tem < tem2						\
	  && cvt_##type1##_##type2 ((type2) 0.75f + type1##_MAX)\
	     != type1##_MAX) abort ();				\
    }								\
  else								\
    {								\
      type2 max = type1##_MAX;					\
      /* tem could be above maximum here due to rounding.  */	\
      MAXT sub = 1;						\
      while (sub)						\
	{							\
	  volatile type2 tem = type1##_MAX - sub;		\
	  if (tem != max)					\
	    break;						\
	  MAXT newsub = sub * type2##_RADIX;			\
	  if (newsub < sub || newsub > type1##_MAX)		\
	    sub = 0;						\
	  else							\
	    sub = newsub;					\
	}							\
      if (sub)							\
	{							\
	  MAXT newsub = ((type1##_MAX - sub) % sub);		\
	  volatile type2 tem = type1##_MAX - newsub;		\
	  volatile type2 tem2 = type1##_MAX - sub;		\
	  if (tem == tem2)					\
	    sub = newsub;					\
	  else							\
	    {							\
	      newsub += sub;					\
	      if (newsub < sub || newsub > type1##_MAX)		\
		sub = 0;					\
	      else						\
		{						\
		  tem = type1##_MAX - newsub;			\
		  if (tem == tem2)				\
		    sub = newsub;				\
		  else						\
		    sub = 0;					\
		}						\
	    }							\
	}							\
      if (sub							\
	  && cvt_##type1##_##type2 (type1##_MAX - sub)		\
	     != type1##_MAX - sub) abort ();			\
    }


#ifdef si128_MAX
# define TESTS128(type2) TEST (si128, type2) TEST (ui128, type2)
#else
# define TESTS128(type2)
#endif

#define TESTS(type2) \
  TEST (sc, type2) TEST (c, type2) TEST (uc, type2)	\
  TEST (ss, type2) TEST (us, type2)			\
  TEST (si, type2) TEST (ui, type2)			\
  TEST (sl, type2) TEST (ul, type2)			\
  TEST (sll, type2) TEST (ull, type2)			\
  TESTS128 (type2)

int
main ()
{
#ifdef f_MAX
  TESTS (f)
#endif
#ifdef d_MAX
  TESTS (d)
#endif
#ifdef ld_MAX
  TESTS (ld)
#endif
#ifdef f80_MAX
  TESTS (f80)
#endif
#ifdef f128_MAX
  TESTS (f128)
#endif
#ifdef BROKEN_DECIMAL_INT128
# undef TESTS128
# define TESTS128(type2)
# undef TWO
# undef M1U
# undef MAXS
# undef MAXT
# define TWO 2ULL
# define M1U -1ULL
# define MAXS (__CHAR_BIT__ * __SIZEOF_LONG_LONG__)
# define MAXT unsigned long long
#endif
#ifdef d32_MAX
  TESTS (d32)
#endif
#ifdef d64_MAX
  TESTS (d64)
#endif
#ifdef d128_MAX
  TESTS (d128)
#endif
  return 0;
}
