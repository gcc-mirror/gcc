extern void abort (void);

#if USE_MEMCMP
/* For comparing vectors.  */
#define TEST_FUNCS(NAME, TYPE, PADT, VAL, VAL2) \
void NAME##_f0 (TYPE a, PADT b)						\
{									\
  if (memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)			\
    abort ();								\
}									\
									\
void NAME##_f1 (PADT z0, TYPE a, PADT b)				\
{									\
  if (z0 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_f2 (PADT z0, PADT z1, TYPE a, PADT b)			\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_f3 (PADT z0, PADT z1, PADT z2, TYPE a, PADT b)		\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_f4 (PADT z0, PADT z1, PADT z2, PADT z3, TYPE a, PADT b)	\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_f5 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, TYPE a,	\
		PADT b)							\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_f6 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, PADT z5,	\
		TYPE a, PADT b)						\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || z5 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_f7 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, PADT z5,	\
		PADT z6, TYPE a, PADT b)				\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || z5 != (PADT) 0							\
      || z6 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_f8 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, PADT z5,	\
		PADT z6, PADT z7, TYPE a, PADT b)			\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || z5 != (PADT) 0							\
      || z6 != (PADT) 0							\
      || z7 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_f9 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, PADT z5,	\
		PADT z6, PADT z7, PADT z8, TYPE a, PADT b)		\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || z5 != (PADT) 0							\
      || z6 != (PADT) 0							\
      || z7 != (PADT) 0							\
      || z8 != (PADT) 0							\
      || memcmp (&a, &VAL, sizeof (a)) != 0 || b != VAL2)		\
    abort ();								\
}									\
									\
void NAME##_fv (int n, ...)						\
{									\
  __builtin_va_list ap;							\
  TYPE x;								\
  __builtin_va_start (ap, n);						\
									\
  while (n-- != 0)							\
    if (__builtin_va_arg (ap, PADT) != (PADT) 0)			\
      abort ();								\
									\
  x = __builtin_va_arg (ap, TYPE);					\
  if (memcmp (&x, &VAL, sizeof (x)) != 0 )				\
    abort ();								\
									\
  if (__builtin_va_arg (ap, PADT) != VAL2)				\
    abort ();								\
									\
  __builtin_va_end (ap);						\
}

#else

#define TEST_FUNCS(NAME, TYPE, PADT, VAL, VAL2) \
void NAME##_f0 (TYPE a, PADT b)						\
{									\
  if (a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f1 (PADT z0, TYPE a, PADT b)				\
{									\
  if (z0 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f2 (PADT z0, PADT z1, TYPE a, PADT b)			\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f3 (PADT z0, PADT z1, PADT z2, TYPE a, PADT b)		\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f4 (PADT z0, PADT z1, PADT z2, PADT z3, TYPE a, PADT b)	\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f5 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, TYPE a,	\
		PADT b)							\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f6 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, PADT z5,	\
		TYPE a, PADT b)						\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || z5 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f7 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, PADT z5,	\
		PADT z6, TYPE a, PADT b)				\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || z5 != (PADT) 0							\
      || z6 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f8 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, PADT z5,	\
		PADT z6, PADT z7, TYPE a, PADT b)			\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || z5 != (PADT) 0							\
      || z6 != (PADT) 0							\
      || z7 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_f9 (PADT z0, PADT z1, PADT z2, PADT z3, PADT z4, PADT z5,	\
		PADT z6, PADT z7, PADT z8, TYPE a, PADT b)		\
{									\
  if (z0 != (PADT) 0							\
      || z1 != (PADT) 0							\
      || z2 != (PADT) 0							\
      || z3 != (PADT) 0							\
      || z4 != (PADT) 0							\
      || z5 != (PADT) 0							\
      || z6 != (PADT) 0							\
      || z7 != (PADT) 0							\
      || z8 != (PADT) 0							\
      || a != VAL || b != VAL2)						\
    abort ();								\
}									\
									\
void NAME##_fv (int n, ...)						\
{									\
  __builtin_va_list ap;							\
									\
  __builtin_va_start (ap, n);						\
									\
  while (n-- != 0)							\
    if (__builtin_va_arg (ap, PADT) != (PADT) 0)			\
      abort ();								\
									\
  if (__builtin_va_arg (ap, TYPE) != VAL)				\
    abort ();								\
									\
  if (__builtin_va_arg (ap, PADT) != VAL2)				\
    abort ();								\
									\
  __builtin_va_end (ap);						\
}

#endif
