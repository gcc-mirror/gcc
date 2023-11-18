/* Based on qemu's util/crc32c.c, in turn based on the
   Linux kernel cryptographic crc32c module.  */

#include <stdint.h>

extern const uint32_t crc32c_table[256];

uint32_t crc32c(uint32_t crc, const uint8_t *data, unsigned int length)
{
    while (length--) {  /* { dg-bogus "infinite loop" } */
        crc = crc32c_table[(crc ^ *data++) & 0xFFL] ^ (crc >> 8);
    }
    return crc^0xffffffff;
}
