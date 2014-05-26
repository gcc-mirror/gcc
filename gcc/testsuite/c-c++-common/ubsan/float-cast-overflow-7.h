#define CVTFN(type1, type2) \
__attribute__((noinline)) type1	\
cvt_##type1##_##type2 (type2 x)	\
{				\
  return x;			\
}

typedef signed char sc;
#define sc_MIN (-__SCHAR_MAX__ - 1)
#define sc_MAX __SCHAR_MAX__
typedef unsigned char uc;
#define uc_MIN 0
#define uc_MAX (2U * __SCHAR_MAX__ + 1U)
typedef char c;
#define c_MIN ((((char) -1) > (char) 0) ? uc_MIN : sc_MIN)
#define c_MAX ((((char) -1) > (char) 0) ? uc_MAX : sc_MAX)
typedef signed short ss;
#define ss_MIN (-__SHRT_MAX__ - 1)
#define ss_MAX __SHRT_MAX__
typedef unsigned short us;
#define us_MIN 0
#define us_MAX (2U * __SHRT_MAX__ + 1U)
typedef signed int si;
#define si_MIN (-__INT_MAX__ - 1)
#define si_MAX __INT_MAX__
typedef unsigned int ui;
#define ui_MIN 0
#define ui_MAX (2U * __INT_MAX__ + 1U)
typedef signed long sl;
#define sl_MIN (-__LONG_MAX__ - 1L)
#define sl_MAX __LONG_MAX__
typedef unsigned long ul;
#define ul_MIN 0L
#define ul_MAX (2UL * __LONG_MAX__ + 1UL)
typedef signed long long sll;
#define sll_MIN (-__LONG_LONG_MAX__ - 1LL)
#define sll_MAX __LONG_LONG_MAX__
typedef unsigned long long ull;
#define ull_MIN 0LL
#define ull_MAX (2ULL * __LONG_LONG_MAX__ + 1ULL)
#ifdef USE_INT128
typedef signed __int128 si128;
# define si128_MAX \
  ((signed __int128) ((((unsigned __int128) 1) \
		       << (__CHAR_BIT__ * __SIZEOF_INT128__ - 1)) - 1))
# define si128_MIN (-si128_MAX - 1)
typedef unsigned __int128 ui128;
#define ui128_MIN ((unsigned __int128) 0)
#define ui128_MAX (((unsigned __int128) 2) * si128_MAX + 1)
#endif

#ifdef si128_MAX
# define CVTS128(type2) CVTFN (si128, type2) CVTFN (ui128, type2)
#else
# define CVTS128(type2)
#endif

#define CVTS(type2) \
  CVTFN (sc, type2) CVTFN (c, type2) CVTFN (uc, type2)	\
  CVTFN (ss, type2) CVTFN (us, type2)			\
  CVTFN (si, type2) CVTFN (ui, type2)			\
  CVTFN (sl, type2) CVTFN (ul, type2)			\
  CVTFN (sll, type2) CVTFN (ull, type2)			\
  CVTS128 (type2)

#ifdef __SIZEOF_INT128__
# define TWO ((unsigned __int128) 2)
# define M1U ((unsigned __int128) -1)
# define MAXS (__CHAR_BIT__ * __SIZEOF_INT128__)
# define MAXT unsigned __int128
#else
# define TWO 2ULL
# define M1U -1ULL
# define MAXS (__CHAR_BIT__ * __SIZEOF_LONG_LONG__)
# define MAXT unsigned long long
#endif

#ifdef USE_FLT_DBL_LDBL
typedef float f;
#define f_RADIX 2
#define f_MANT_DIG __FLT_MANT_DIG__
#define f_MAX ((TWO << (f_MANT_DIG - 1)) - 1)
typedef double d;
#define d_RADIX 2
#define d_MANT_DIG __DBL_MANT_DIG__
#define d_MAX ((TWO << (d_MANT_DIG - 1)) - 1)
typedef long double ld;
#define ld_RADIX 2
#define ld_MANT_DIG __LDBL_MANT_DIG__
#define ld_MAX \
  (ld_MANT_DIG > MAXS ? M1U : (TWO << (ld_MANT_DIG > MAXS \
				       ? 0 : ld_MANT_DIG - 1)) - 1)
CVTS (f)
CVTS (d)
CVTS (ld)
#endif
#ifdef USE_FLOAT80
typedef __float80 f80;
# define f80_RADIX 2
# define f80_MANT_DIG 64
# define f80_MAX ((TWO << (f80_MANT_DIG - 1)) - 1)
CVTS (f80)
#endif
#ifdef USE_FLOAT128
typedef __float128 f128;
# define f128_RADIX 2
# define f128_MANT_DIG 113
# define f128_MAX \
  (f128_MANT_DIG > MAXS ? M1U : (TWO << (f128_MANT_DIG > MAXS \
					 ? 0 : f128_MANT_DIG - 1)) - 1)
CVTS (f128)
#endif
#ifdef USE_DFP
# ifdef __cplusplus
typedef float _Decimal32 __attribute__((mode(SD)));
typedef float _Decimal64 __attribute__((mode(DD)));
typedef float _Decimal128 __attribute__((mode(TD)));
# endif
typedef _Decimal32 d32;
# define d32_RADIX 10
# define d32_MANT_DIG __DEC32_MANT_DIG__
# if d32_MANT_DIG == 7
#  define d32_MAX 9999999ULL
# endif
typedef _Decimal64 d64;
# define d64_RADIX 10
# define d64_MANT_DIG __DEC64_MANT_DIG__
# if d64_MANT_DIG == 16
#  define d64_MAX 9999999999999999ULL
# endif
typedef _Decimal128 d128;
# define d128_RADIX 10
# define d128_MANT_DIG __DEC128_MANT_DIG__
# if d128_MANT_DIG == 34
#  ifdef __SIZEOF_INT128__
/* #define d128_MAX 0x1ed09bead87c0378d8e63ffffffff */
#   define d128_MAX \
  ((((unsigned __int128) 0x1ed09bead87c0) << 64) + 0x378d8e63ffffffffULL)
#  else
#   define d128_MAX M1U
#  endif
# endif
# ifdef BROKEN_DECIMAL_INT128
#  undef CVTS128
#  define CVTS128(type2)
# endif
CVTS (d32)
CVTS (d64)
CVTS (d128)
#endif

extern
#ifdef __cplusplus
"C"
#endif
void abort ();
