/* { dg-do run { target { rv32 || rv64 } } } */
/* { dg-options "-O2" } */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <array>
#include <limits>
#include <memory>
#include <new>
#include <type_traits>

#define HWY_INLINE inline __attribute__((__always_inline__))
#define HWY_MIN(a, b) ((a) < (b) ? (a) : (b))
#define HWY_MAX(a, b) ((a) > (b) ? (a) : (b))

#if defined(__GNUC__) && !defined(__clang__)
#define NOIPA_ATTR __attribute__((__noipa__))
#else
#define NOIPA_ATTR
#endif

namespace test {

static __attribute__((__noinline__)) NOIPA_ATTR int Unpredictable1() {
  int result = 1;
  __asm__("" : "+r"(result)::);
  return result;
}

class RandomState {
 public:
  explicit RandomState(
      const uint64_t seed = uint64_t{0x123456789} *
                            static_cast<uint64_t>(test::Unpredictable1())) {
    s0_ = SplitMix64(seed + 0x9E3779B97F4A7C15ull);
    s1_ = SplitMix64(s0_);
  }

  HWY_INLINE uint64_t operator()() {
    uint64_t s1 = s0_;
    const uint64_t s0 = s1_;
    const uint64_t bits = s1 + s0;
    s0_ = s0;
    s1 ^= s1 << 23;
    s1 ^= s0 ^ (s1 >> 18) ^ (s0 >> 5);
    s1_ = s1;
    return bits;
  }

 private:
  static uint64_t SplitMix64(uint64_t z) {
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
    return z ^ (z >> 31);
  }

  uint64_t s0_;
  uint64_t s1_;
};

static __attribute__((__noinline__)) NOIPA_ATTR void GenerateRandomU16x16Vals(
    RandomState& rng, uint16_t* const from, uint8_t* const expected) {
  using T = uint16_t;
  using TN = uint8_t;

  const T min = 0;
  const T max = static_cast<T>(std::numeric_limits<TN>::max());

  for (size_t i = 0; i < 16; ++i) {
    const uint64_t bits = rng();
    __builtin_memcpy(&from[i], &bits, sizeof(T));  // not same size
    expected[i] = static_cast<TN>(HWY_MIN(HWY_MAX(min, from[i]), max));
  }
}

static __attribute__((__noinline__)) NOIPA_ATTR void DoVerifyU16x16Demote(
    const uint16_t* const from, const uint8_t* const expected) {
  for (int i = 0; i < 16; ++i) {
    const uint8_t actual =
        static_cast<uint8_t>((from[i] < 0xFF) ? from[i] : 0xFF);
    if (expected[i] != actual) {
      fprintf(stderr,
              "Mismatch between expected result and actual result\nfrom=%u, "
              "expected=%u, actual=%u\n",
              static_cast<unsigned>(from[i]),
              static_cast<unsigned>(expected[i]),
              static_cast<unsigned>(actual));
      __builtin_abort();
    }
  }
}

static void DoDemoteU16x16ToU8x16Test() {
  using T = uint16_t;
  using TN = uint8_t;
  std::array<T, 16> from;
  std::array<TN, 16> expected;

  RandomState rng;
  for (size_t rep = 0; rep < 1000; ++rep) {
    GenerateRandomU16x16Vals(rng, from.data(), expected.data());
    DoVerifyU16x16Demote(from.data(), expected.data());
  }
}

}  // namespace test

int main(int /*argc*/, char** /*argv*/) {
  printf("Doing DoDemoteU16x16ToU8x16Test\n");
  test::DoDemoteU16x16ToU8x16Test();
  printf("Test completed successfully\n");
  return 0;
}
