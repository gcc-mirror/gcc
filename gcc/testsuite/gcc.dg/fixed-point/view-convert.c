/* PR tree-optimization/56064 */
/* { dg-do run } */
/* { dg-options "-std=gnu99 -O2 -fno-builtin-memcpy" } */

extern void abort (void);
extern void *memcpy (void*, const void*, __SIZE_TYPE__);

#define f_pun_i(F, I, VAL)                      \
  {                                             \
    I i1 = VAL;                                 \
    I i2 = VAL;                                 \
    F q1, q2;                                   \
    memcpy (&q1, &i1, sizeof (I));              \
    __builtin_memcpy (&q2, &i2, sizeof (I));    \
    if (q1 != q2)                               \
      abort();                                  \
  }

#define i_pun_f(I, F, VAL)                      \
  {                                             \
    F q1 = VAL;                                 \
    F q2 = VAL;                                 \
    I i1, i2;                                   \
    memcpy (&i1, &q1, sizeof (I));              \
    __builtin_memcpy (&i2, &q2, sizeof (I));    \
    if (i1 != i2)                               \
      abort();                                  \
  }


void __attribute__((noinline))
test8 (void)
{
#ifdef __INT8_TYPE__
  if (sizeof (__INT8_TYPE__) == sizeof (short _Fract))
    {
#define TEST(X) f_pun_i (short _Fract, __INT8_TYPE__, __INT8_C (X))
      TEST (123);
      TEST (-123);
#undef TEST

#define TEST(X) i_pun_f (__INT8_TYPE__, short _Fract, X ## hr)
      TEST (0.1234);
      TEST (-0.987);
#undef TEST
    }
#endif /* __INT8_TYPE__ */
}


void __attribute__((noinline))
test16 (void)
{
#ifdef __INT16_TYPE__

  if (sizeof (__INT16_TYPE__) == sizeof (_Fract))
    {
#define TEST(X) f_pun_i (_Fract, __INT16_TYPE__, __INT16_C (X))
      TEST (0x4321);
      TEST (-0x4321);
      TEST (0x8000);
#undef TEST

#define TEST(X) i_pun_f (__INT16_TYPE__, _Fract, X ## r)
      TEST (0.12345);
      TEST (-0.98765);
#undef TEST
    }
#endif /* __INT16_TYPE__ */
}


void __attribute__((noinline))
test32 (void)
{
#ifdef __INT32_TYPE__
  if (sizeof (__INT32_TYPE__) == sizeof (_Accum))
    {
#define TEST(X) f_pun_i (_Accum, __INT32_TYPE__, __INT32_C (X))
      TEST (0x76543219);
      TEST (-0x76543219);
      TEST (0x80000000);
#undef TEST

#define TEST(X) i_pun_f (__INT32_TYPE__, _Accum, X ## k)
      TEST (123.456789);
      TEST (-123.456789);
#undef TEST
    }
#endif /* __INT32_TYPE__ */
}


void __attribute__((noinline))
test64 (void)
{
#ifdef __INT64_TYPE__
  if (sizeof (__INT64_TYPE__) == sizeof (long _Accum))
    {
#define TEST(X) f_pun_i (long _Accum, __INT64_TYPE__, __INT64_C (X))
      TEST (0x12345678abcdef01);
      TEST (-0x12345678abcdef01);
      TEST (0x8000000000000000);
#undef TEST

#define TEST(X) i_pun_f (__INT64_TYPE__, long _Accum, X ## lk)
      TEST (123.456789);
      TEST (-123.456789);
#undef TEST
    }
#endif /* __INT64_TYPE__ */
}

int main()
{
  test8();
  test16();
  test32();
  test64();
  
  return 0;
}
