/* builtin_frame_address(n) with n>0 has always been troublesome ...
   especially when the S/390 packed stack layout comes into play.  */

/* { dg-do run } */
/* { dg-options "-O3 -fno-optimize-sibling-calls -mbackchain -mpacked-stack -msoft-float" } */

#ifdef __s390x__
/* 64bit: 3 words to be saved: backchain, r14 and r15  */
#define SAVE_AREA_SIZE 3*8
#else
/* 32bit: 4 words to be saved: backchain, r13, r14 and r15  */
#define SAVE_AREA_SIZE 4*4
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
