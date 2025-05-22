[[gnu::noipa]] void
do_copy (void *p, const void *q, __SIZE_TYPE__ r)
{
  __builtin_memcpy (p, q, r);
}

/* Macro to test whether (on targets where psABI requires it) _BitInt
   with padding bits have those filled with sign or zero extension.  */
#if defined(__s390x__) || defined(__arm__) || defined(__loongarch__)
#define BEXTC(x) \
  do {								\
    if ((typeof (x)) -1 < 0)					\
      {								\
	_BitInt(sizeof (x) * __CHAR_BIT__) __x;			\
	do_copy (&__x, &(x), sizeof (__x));			\
	if (__x != (x))						\
	  __builtin_abort ();					\
      }								\
    else							\
      {								\
	unsigned _BitInt(sizeof (x) * __CHAR_BIT__) __x;	\
	do_copy (&__x, &(x), sizeof (__x));			\
	if (__x != (x))						\
	  __builtin_abort ();					\
      }								\
  } while (0)
#else
#define BEXTC(x) do { (void) (x); } while (0)
#endif
