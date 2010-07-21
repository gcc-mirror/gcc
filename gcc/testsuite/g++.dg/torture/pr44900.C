/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse" } */
/* { dg-require-effective-target sse } */

typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef float __v4sf __attribute__ ((__vector_size__ (16)));

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__,
__artificial__))
_mm_set_ps (const float __Z, const float __Y, const float __X, const float __W)
{
  return __extension__ (__m128)(__v4sf){ __W, __X, __Y, __Z };
}

struct vec
{
        union {
                __m128 v;
                float  e[4];
        };

        static const vec & zero()
        {
                static const vec v = _mm_set_ps(0, 0, 0, 0);
                return v;
        }

        vec() {}
        vec(const __m128 & a) : v(a) {}

        operator const __m128&() const { return v; }
};

struct vec2
{
        vec _v1;
        vec _v2;

        vec2() {}
        vec2(const vec & a, const vec & b) : _v1(a), _v2(b) {}

        static vec2 load(const float * a)
        {
                return vec2(
                        __builtin_ia32_loadups(&a[0]),
                        __builtin_ia32_loadups(&a[4]));
        }

        const vec & v1() const { return _v1; }
        const vec & v2() const { return _v2; }
};

extern "C" void abort(void);


inline bool operator==(const vec & a, const vec & b)
{ return 0xf == __builtin_ia32_movmskps(__builtin_ia32_cmpeqps(a, b)); }

int main( int argc, char * argv[] )
{
        __attribute__((aligned(16))) float data[] =
        { 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5 };

        float * p = &data[2];
        vec2 a;

        a = vec2::load(p);

        vec v1 = a.v1();
        vec v2 = a.v2();

	if (v2.e[3] != 7.0)
	  abort();

        return 0;
}
