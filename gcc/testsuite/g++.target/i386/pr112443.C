/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O2 -std=c++17 -mavx512bw -mavx512vl" } */

#include <cstdint>
#include <x86intrin.h>
#include <functional>
#include <ostream>

#define AVX512BW
#define AVX512VL

#include "avx512f-helper.h"

struct TensorIteratorBase{
  char* in;
  char* out;

  void for_each(std::function<void(char*, char*, int64_t size)> loop){
    loop(out, in, 32);
  }    
};

class Vectorized {
protected:
  __m256i values;

  static inline __m256i invert(const __m256i& v) {
    const auto ones = _mm256_set1_epi64x(-1);
    return _mm256_xor_si256(ones, v);
  }
public:
  operator __m256i() const {
    return values;
  }

  static constexpr int size() {
    return 32;
  }

  Vectorized() {}
  Vectorized(__m256i v) : values(v) {}
  Vectorized(uint8_t v) { values = _mm256_set1_epi8(v); }
  static Vectorized blendv(const Vectorized& a, const Vectorized& b,
			   const Vectorized& mask) {
    return _mm256_blendv_epi8(a, b, mask);
  }
  static Vectorized loadu(const void* ptr) {
    return _mm256_loadu_si256(reinterpret_cast<const __m256i*>(ptr));
  }
  void store(void* ptr) const {
    _mm256_storeu_si256(reinterpret_cast<__m256i*>(ptr), values);
  }

  Vectorized operator<(const Vectorized& other) const {
    __m256i max = _mm256_max_epu8(values, other);
    return invert(_mm256_cmpeq_epi8(max, values));
  }
  Vectorized operator-(const Vectorized& b) {
    return _mm256_sub_epi8(values, b);
  }
};

std::ostream& operator<<(std::ostream& stream, const Vectorized& vec) {
  uint8_t buf[Vectorized::size()];
  vec.store(buf);
  stream << "vec[";
  for (int i = 0; i != Vectorized::size(); i++) {
    if (i != 0)
      stream << ", ";
    stream << buf[i]*1;
  }
  stream << "]";
  return stream;
}

void run(TensorIteratorBase iter){
  Vectorized zero_vec(0);
  Vectorized one_vec(1);

  iter.for_each([=](char* out, char* in, int64_t size) {
    for (int64_t i = 0; i <= size - Vectorized::size(); i += Vectorized::size()) {
      auto self_vec = Vectorized::loadu(in + i);
      auto left = Vectorized::blendv(zero_vec, one_vec, zero_vec < self_vec);
      auto right = Vectorized::blendv(zero_vec, one_vec, self_vec < zero_vec);
      auto outv = left - right;
      outv.store(out + i);
    }
  });
}

void
test_256 (){
  char in[32];
  char out[32];
  for(auto& x: in) x = 1;
  run(TensorIteratorBase{in, out});
  Vectorized::loadu (out);
  for (int i = 0; i != 32; i++)
    if (out[i] != 1)
      __builtin_abort ();
}

void
test_128 ()
{
}
