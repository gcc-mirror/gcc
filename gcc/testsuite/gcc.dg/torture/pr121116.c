/* { dg-do run { target bitint } } */

#include <stdlib.h>
#include <stdckdint.h>
#include <string.h>
typedef _BitInt(16) bit16;
[[nodiscard]] static bit16 process_data(bit16 input) {
    _Static_assert(sizeof(bit16) == 2, "Unexpected size of bit16");
    return (input << 5) | (input >> 9);
}
int main(void) {
    const bit16 data = 0b101'0101'0000'0000;
    bit16 result = 0;
    for (bit16 i = 0; i < 0b1000; ++i) {
        result ^= process_data(data ^ i);
    }
    if (ckd_add(&result, result, 0x1234)) {
        return EXIT_FAILURE;
    }
    return (result & 0xFF00) ? 0 : 1;
}
