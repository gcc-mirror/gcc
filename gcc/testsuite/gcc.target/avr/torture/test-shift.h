#define NI __attribute__((noipa))

NI
T shiftN (T x, int n)
{
  return x OP n;
}

#define MK_FUN(N)				\
  NI						\
  T shift_##N##_r22 (T x)			\
  {						\
    return x OP N;				\
  }						\
  						\
  NI						\
  T shift_##N##_r20 (T x)			\
  {						\
    register T r20 __asm("20") = x OP N;	\
    __asm ("nop ; %0" : "+r" (r20));		\
    return r20;					\
  }						\
						\
  NI						\
  T shift_##N##_r24 (T x)			\
  {						\
    register T r24 __asm("24") = x OP N;	\
    __asm ("nop ; %0" : "+r" (r24));		\
    return r24;					\
  }						\

MK_FUN (1)
MK_FUN (2)
MK_FUN (3)
MK_FUN (4)
MK_FUN (5)
MK_FUN (6)
MK_FUN (7)

#if BITS > 8
MK_FUN (8)
MK_FUN (9)
MK_FUN (10)
MK_FUN (11)
MK_FUN (12)
MK_FUN (13)
MK_FUN (14)
MK_FUN (15)
#endif

#if BITS > 16
MK_FUN (16)
MK_FUN (17)
MK_FUN (18)
MK_FUN (19)
MK_FUN (20)
MK_FUN (21)
MK_FUN (22)
MK_FUN (23)
#endif

#if BITS > 24
MK_FUN (24)
MK_FUN (25)
MK_FUN (26)
MK_FUN (27)
MK_FUN (28)
MK_FUN (29)
MK_FUN (30)
MK_FUN (31)
#endif

#define ARRAY_SIZE(X) ((int) (sizeof (X) / sizeof (*X)))

#ifdef __FLASH
#define AS __flash
#else
#define AS /* empty */
#endif

typedef T (*fun_t) (T);

typedef struct
{
  fun_t f[3];
} fun3_t;

#define FN(N) { { shift_##N##_r20, shift_##N##_r22, shift_##N##_r24 } }

const AS fun3_t funcs[] =
  {
    FN (1), FN (2), FN (3), FN (4), FN (5), FN (6), FN (7),
#if BITS > 8
    FN (8), FN (9), FN (10), FN (11), FN (12), FN (13), FN (14), FN (15),
#endif
#if BITS > 16
    FN (16), FN (17), FN (18), FN (19), FN (20), FN (21), FN (22), FN (23),
#endif
#if BITS > 24
    FN (24), FN (25), FN (26), FN (27), FN (28), FN (29), FN (30), FN (31),
#endif
  };

void test1 (fun_t fun, T x, int n)
{
  T res = shiftN (x, n);
  if (res != fun (x))
    __builtin_abort ();
}

void testx (T x)
{
  for (int i = 0; i < ARRAY_SIZE (funcs); ++i)
    {
      const int n = 1 + i;
      const T res = shiftN (x, n);

      for (int j = 0; j < ARRAY_SIZE (funcs[i].f); ++j)
	{
	  fun_t f = funcs[i].f[j];
	  if (f (x) != res)
	    __builtin_abort ();
	}
    }
}
