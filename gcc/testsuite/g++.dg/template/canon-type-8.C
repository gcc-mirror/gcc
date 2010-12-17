// PR c++/45984
// We were getting different canonical types for matching types because
// TYPE_ALIGN wasn't propagated to all the variants fast enough.
// { dg-options "" }

typedef __SIZE_TYPE__ size_t;
enum { chunk_size = 16 };
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef float __v4sf __attribute__ ((__vector_size__ (16)));
struct __attribute__((aligned((16)))) float4_t {
    typedef float scalar_t;
    typedef __m128 type_t;
    typedef float4_t return_type_t;
    type_t m;
    inline __attribute__((artificial, gnu_inline, always_inline)) explicit
    float4_t(scalar_t a) : m(((__m128) (__v4sf) { (a), (a), (a), (a) })) { }
    inline __attribute__((artificial, gnu_inline, always_inline, pure)) friend
    return_type_t operator+(float4_t lhs, float4_t rhs) { }
};
template<size_t NumChans>  class __attribute__((aligned((16)))) chunk_array_t {
public:
    typedef float4_t value_type_t;
    typedef value_type_t value_array_t[chunk_size/4];
    enum { num_scalars = chunk_size,    num_values = num_scalars/4  };
    const value_array_t &chan(size_t c) const { }
    value_type_t operator[](size_t i) const { }
};
typedef chunk_array_t<1> chunk_array_mono_t;
typedef chunk_array_t<2> chunk_array_stereo_t;
class freeverb_stereo_t {
    void process(const chunk_array_stereo_t & __restrict__ src,
                 chunk_array_stereo_t & __restrict__ dst) {
        enum { chunk_size = chunk_array_t<1>::num_values };
        chunk_array_mono_t mix;
        for (size_t i=0; i<chunk_size; ++i)
          mix[i] = src.chan(0)[i] + src.chan(1)[i];
    }
};
