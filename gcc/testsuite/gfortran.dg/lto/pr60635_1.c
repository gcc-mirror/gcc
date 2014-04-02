#include <stdint.h>
#include <stdbool.h>

static bool littleendian=true;

uint16_t bigendc16(union{uint16_t * n;uint8_t* b;}x){

    if (!littleendian) return *x.n;

    uint16_t res = ((uint16_t)(x.b[1])<<0) |
	((uint16_t)(x.b[0])<<8);
    return res;
}

