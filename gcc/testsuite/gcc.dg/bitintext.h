[[gnu::noipa]] void
do_copy (void *p, const void *q, __SIZE_TYPE__ r)
{
  __builtin_memcpy (p, q, r);
}

/* Obtain the value of N from a _BitInt(N)-typed expression X
   at compile time.  */
#define S(x) \
  ((typeof (x)) -1 < 0                                                   \
   ? __builtin_clrsbg (__builtin_choose_expr ((typeof (x)) -1 < 0,       \
					      (typeof (x)) -1, -1)) + 1  \
   : __builtin_popcountg (__builtin_choose_expr ((typeof (x)) -1 < 0,    \
						 0U, (typeof (x)) -1)))

#define CEIL(x,y) (((x) + (y) - 1) / (y))

/* Promote a _BitInt type to include its padding bits.  */
#if defined (__s390x__) || defined(__arm__)
#define PROMOTED_SIZE(x) sizeof (x)
#elif defined(__loongarch__)
#define PROMOTED_SIZE(x) (sizeof (x) > 8 ? CEIL (S (x), 64) * 8 : sizeof (x))
#endif

/* Macro to test whether (on targets where psABI requires it) _BitInt
   with padding bits have those filled with sign or zero extension.  */
#if defined(__s390x__) || defined(__arm__) || defined(__loongarch__)
#define BEXTC1(x, uns) \
  do {							  \
    uns _BitInt(PROMOTED_SIZE (x) * __CHAR_BIT__) __x;	  \
    do_copy (&__x, &(x), sizeof (__x));			  \
    if (__x != (typeof (x)) __x)			  \
      __builtin_abort ();				  \
  } while (0)

#define BEXTC(x) \
  do {				\
    if ((typeof (x)) -1 < 0)	\
      BEXTC1 ((x), signed);	\
    else			\
      BEXTC1 ((x), unsigned);	\
  } while (0)
#else
#define BEXTC(x) do { (void) (x); } while (0)
#endif
