/* { dg-do run } */

#include <stddef.h>
#include <stdint.h>

struct ve {
    ve() = default;
    ve(const ve&) = default;
    ve& operator=(const ve&) = default;

    // note that the code usually uses the first half of this array
    uint8_t raw[16] = {};
};

static ve First8_(void) {
    ve m;
    __builtin_memset(m.raw, 0xff, 8);
    return m;
}

static ve And_(ve a, ve b) {
    ve au;
    __builtin_memcpy(au.raw, a.raw, 16);
    for (size_t i = 0; i < 8; ++i) {
        au.raw[i] &= b.raw[i];
    }
    return au;
}

__attribute__((noipa, optimize(0)))
static void vec_assert(ve a) {
    if (a.raw[6] != 0x06 && a.raw[6] != 0x07)
        __builtin_trap();
}

static ve Reverse4_(ve v) {
    ve ret;
    for (size_t i = 0; i < 8; i += 4) {
        ret.raw[i + 0] = v.raw[i + 3];
        ret.raw[i + 1] = v.raw[i + 2];
        ret.raw[i + 2] = v.raw[i + 1];
        ret.raw[i + 3] = v.raw[i + 0];
    }
    return ret;
}

static ve DupEven_(ve v) {
    for (size_t i = 0; i < 8; i += 2) {
        v.raw[i + 1] = v.raw[i];
    }
    return v;
}

template <bool b>
ve Per4LaneBlockShuffle_(ve v) {
    if (b) {
        return Reverse4_(v);
    } else {
        return DupEven_(v);
    }
}

template <bool b>
static inline __attribute__((always_inline)) void DoTestPer4LaneBlkShuffle(const ve v) {
    ve actual = Per4LaneBlockShuffle_<b>(v);
    const auto valid_lanes_mask = First8_();
    ve actual_masked = And_(valid_lanes_mask, actual);
    vec_assert(actual_masked);
}

static void DoTestPer4LaneBlkShuffles(const ve v) {
    alignas(128) uint8_t src_lanes[8];
    __builtin_memcpy(src_lanes, v.raw, 8);
    // need both, hm
    DoTestPer4LaneBlkShuffle<true >(v);
    DoTestPer4LaneBlkShuffle<false>(v);
}

__attribute__((noipa, optimize(0)))
static void bug(void) {
   uint8_t iv[8] = {1,2,3,4,5,6,7,8};
   ve v;
   __builtin_memcpy(v.raw, iv, 8);
   DoTestPer4LaneBlkShuffles(v);
}

int main(void) {
    bug();
}

