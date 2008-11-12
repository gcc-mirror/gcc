/* builtin_frame_address(n) with n>0 has always been troublesome.  */

/* { dg-do run } */
/* { dg-options "-O3 -fno-optimize-sibling-calls -mbackchain" } */

#ifdef __s390x__
#define SAVE_AREA_SIZE 160
#else
#define SAVE_AREA_SIZE 96
#endif
extern void abort(void);

#define EXPAND_CHECK(n)						\
  void __attribute__((noinline))				\
    foo1_##n (void *p)						\
  {								\
    if (p - __builtin_frame_address (n) != SAVE_AREA_SIZE)	\
      abort ();							\
  }								\
  void __attribute__((noinline))				\
    foo2_##n (void *p)						\
  {								\
    if (p - __builtin_frame_address (n) != SAVE_AREA_SIZE)	\
      abort ();							\
    foo1_##n (__builtin_frame_address (n));			\
  }								\
  void __attribute__((noinline))				\
    foo3_##n ()							\
  {								\
    foo2_##n (__builtin_frame_address (n));			\
  }								\
  void __attribute__((noinline))				\
    foo4_##n ()							\
  {								\
    foo3_##n ();						\
  }

EXPAND_CHECK (0)
EXPAND_CHECK (1)
EXPAND_CHECK (2)

int
main ()
{
  foo4_0 ();
  foo4_1 ();
  foo4_2 ();

  return 0;
}
