// { dg-do compile }
// { dg-additional-options "-Wno-psabi" }

class AssertionResult {
    bool success_;
};

AssertionResult AssertionSuccess();

template <typename T1>
AssertionResult EXPECT_EQ(const T1& expected, const T1& actual) {
    if (expected == actual) {
	return AssertionSuccess();
    }
    return AssertionSuccess();
}

struct uuid
{
  unsigned char data[16];
};

bool operator== (uuid const& lhs, uuid const& rhs);

typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef char __v16qi __attribute__ ((__vector_size__ (16)));
typedef long long __m128i_u __attribute__ ((__vector_size__ (16), __may_alias__, __aligned__ (1)));

int foo (__v16qi);

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
     _mm_loadu_si128 (__m128i_u const *__P)
{
    return *__P;
}
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
     _mm_cmpeq_epi32 (__m128i __A, __m128i __B)
{
    return (__m128i) ((__v4si)__A == (__v4si)__B);
}
extern __inline int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
     _mm_movemask_epi8 (__m128i __A)
{
    return foo ((__v16qi)__A);
}


__m128i load_unaligned_si128(const unsigned char* p)
{
  return _mm_loadu_si128(reinterpret_cast< const __m128i* >(p));
}

inline bool operator== (uuid const& lhs, uuid const& rhs)
{
  __m128i mm_left = load_unaligned_si128(lhs.data);
  __m128i mm_right = load_unaligned_si128(rhs.data);

  __m128i mm_cmp = _mm_cmpeq_epi32(mm_left, mm_right);

  return _mm_movemask_epi8(mm_cmp) == 0xFFFF;
}

void crash_gcc7()
{
  static const uuid u = uuid();
  EXPECT_EQ(u, u);
}
