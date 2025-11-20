/* { dg-do run { target { rv32 || rv64 } } } */
/* { dg-options "-O2" } */

#include <inttypes.h>
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

template <class T>
static __attribute__((noinline)) NOIPA_ATTR T* MallocArray(
    size_t num_to_alloc) {
  static_assert(sizeof(T) > 0, "sizeof(T) > 0 must be true");
  constexpr size_t kMaxNumToAlloc =
      std::numeric_limits<size_t>::max() / sizeof(T);
  if (num_to_alloc > kMaxNumToAlloc) {
    return nullptr;
  }

  return reinterpret_cast<T*>(::malloc(num_to_alloc * sizeof(T)));
}

struct CFreeDeleter {
  HWY_INLINE void operator()(const volatile void* ptr) const noexcept {
    if (ptr) {
      ::free(const_cast<void*>(ptr));
    }
  }
};

#define HWY_ASSERT(cond)                                              \
  do {                                                                \
    if (__builtin_expect(!(cond), false)) {                           \
      fprintf(stderr, "Assertion failed at line %d of file %s: %s\n", \
              static_cast<int>(__LINE__), __FILE__, "" #cond);        \
      fflush(stderr);                                                 \
      __builtin_abort();                                              \
    }                                                                 \
  } while (false)

static __attribute__((__noinline__)) NOIPA_ATTR void AssertU8x16ArrayEquals(
    std::array<uint8_t, 16> expected, std::array<uint8_t, 16> actual,
    const int line, const char* filename) {
  for (size_t i = 0; i < 16; i++) {
    if (expected[i] != actual[i]) {
      fprintf(stderr, "Array mismatch at line %d of file %s:\n", line,
              filename);
      fprintf(stderr,
              "Expected: {%" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8
              ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8
              ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8
              ", %" PRIu8 ", %" PRIu8 "}\n",
              expected[0], expected[1], expected[2], expected[3], expected[4],
              expected[5], expected[6], expected[7], expected[8], expected[9],
              expected[10], expected[11], expected[12], expected[13],
              expected[14], expected[15]);
      fprintf(stderr,
              "Actual:   {%" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8
              ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8
              ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8
              ", %" PRIu8 ", %" PRIu8 "}\n",
              actual[0], actual[1], actual[2], actual[3], actual[4], actual[5],
              actual[6], actual[7], actual[8], actual[9], actual[10],
              actual[11], actual[12], actual[13], actual[14], actual[15]);
      __builtin_abort();
    }
  }
}

#define ASSERT_U8X16_ARR_EQUALS(expected, actual) \
  AssertU8x16ArrayEquals(expected, actual, __LINE__, __FILE__)

static std::array<uint8_t, 16> LoadU8x16Vec(const uint8_t* __restrict ptr) {
  std::array<uint8_t, 16> result;
  __builtin_memcpy(&result, ptr, 16 * sizeof(uint8_t));
  return result;
}

static std::array<uint16_t, 8> LoadU16x8Vec(const uint16_t* __restrict ptr) {
  std::array<uint16_t, 8> result;
  __builtin_memcpy(&result, ptr, 8 * sizeof(uint16_t));
  return result;
}

static void DoOrderedDemote2U16x8ToU8x16Test() {
  using T = uint16_t;
  using TN = uint8_t;
  std::unique_ptr<T[], CFreeDeleter> from(MallocArray<T>(16));
  std::unique_ptr<TN[], CFreeDeleter> expected(MallocArray<TN>(16));
  HWY_ASSERT(from && expected);

  constexpr size_t N = 8;
  constexpr size_t twiceN = 16;

  // Narrower range in the wider type, for clamping before we cast
  const T min = static_cast<T>(
      std::is_signed_v<T> ? std::numeric_limits<TN>::lowest() : TN{0});
  const T max = std::numeric_limits<TN>::max();

  RandomState rng;
  for (size_t rep = 0; rep < 1000; ++rep) {
    for (size_t i = 0; i < twiceN; ++i) {
      const uint64_t bits = rng();
      __builtin_memcpy(&from[i], &bits, sizeof(T));  // not same size
      expected[i] = static_cast<TN>(HWY_MIN(HWY_MAX(min, from[i]), max));
    }

    std::array<uint8_t, 16> actual;
    for (size_t i = 0; i < 16; i++) {
      actual[i] = static_cast<uint8_t>(HWY_MIN(from[i], 0xFF));
    }
    ASSERT_U8X16_ARR_EQUALS(LoadU8x16Vec(expected.get()), actual);
  }
}

}  // namespace test

int main(int /*argc*/, char** /*argv*/) {
  printf("Doing DoOrderedDemote2U16x8ToU8x16Test\n");
  test::DoOrderedDemote2U16x8ToU8x16Test();
  printf("Test completed successfully\n");
  return 0;
}
