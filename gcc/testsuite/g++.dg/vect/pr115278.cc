// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-fdump-tree-optimized" }

#include <cstdint>

const int runs = 92;

union BitfieldStructUnion {
        struct {
                uint64_t a : 17;
        uint64_t padding: 39;
                uint64_t b : 8;
        } __attribute__((packed));

        struct {
                uint32_t value_low;
                uint32_t value_high;
        } __attribute__((packed));

        BitfieldStructUnion(uint32_t value_low, uint32_t value_high) : value_low(value_low), value_high(value_high) {}
};

volatile uint32_t *WRITE = (volatile unsigned*)0x42;

void buggy() {
        for (int i = 0; i < runs; i++) {
                BitfieldStructUnion rt{*WRITE, *WRITE};

                rt.a = 99;
                rt.b = 1;

                *WRITE = rt.value_low;
                *WRITE = rt.value_high;
        }
}

// { dg-final { scan-tree-dump-times "\\\*WRITE\[^\r\n\]* ={v} " 2 "optimized" } }
