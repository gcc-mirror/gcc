/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-O3 -msse2 -mfpmath=sse" } */

typedef unsigned short int __uint16_t;
typedef unsigned int __uint32_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_store_si128 (__m128i *__P, __m128i __B)
{
  *__P = __B;
}
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_set_epi32 (int __q3, int __q2, int __q1, int __q0)
{
  return __extension__ (__m128i)(__v4si){ __q0, __q1, __q2, __q3 };
}
typedef uint16_t u16;
typedef uint32_t u32;
extern int printf (const char *__restrict __format, ...);
void do_the_thing(u32 idx, __m128i *dude)
{
 u32 dude_[4] = { idx+0, idx+2, idx+4, idx+6 };
 for (u32 i = 0; i < 3; ++i)
  if (dude_[i] == 1234)
   dude_[i]--;
 *dude = _mm_set_epi32(dude_[0], dude_[1], dude_[2], dude_[3]);
}
int main()
{
 __m128i dude;
 u32 idx = 0;
 do_the_thing(idx, &dude);
 __attribute__((aligned(16))) u32 dude_[4];
 _mm_store_si128((__m128i*)dude_, dude);
 if (!(6 == dude_[0] && 4 == dude_[1] && 2 == dude_[2] && 0 == dude_[3]))
   __builtin_abort ();
 return 0;
}
