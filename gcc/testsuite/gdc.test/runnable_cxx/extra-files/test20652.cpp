#if defined(__DMC__)	// DMC doesn't support immintrin.h
#else

#include <assert.h>

// Inline the typedef of __m128 instead of including immintrin.h.
#if defined(__GNUC__) || defined(__clang__)
typedef float __m128 __attribute__((__vector_size__(16), __may_alias__));

#elif defined(_MSC_VER)
typedef union __declspec(intrin_type) __declspec(align(16)) __m128 {
    float m128_f32[4];
} __m128;

#else
#error "Unknown vendor"
#endif

void test20652(const __m128& a)
{
    union
    {
        __m128 value;
        float array[4];
    } b;
    b.value = a;

    assert(b.array[0] == 1);
    assert(b.array[1] == 1);
    assert(b.array[2] == 1);
    assert(b.array[3] == 1);
}

#endif
