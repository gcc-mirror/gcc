/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

// File - slz
#include <stdint.h>
#include <stdio.h>
uint32_t crc32_fast[4][256];
void __slz_make_crc_table(void) {
    uint32_t c;
    int n, k;

    for (n = 0; n < 256; n++) {
        c = (uint32_t) n ^ 255;
        for (k = 0; k < 8; k++) {
            if (c & 1) {
                c = 0xedb88320 ^ (c >> 1);
            } else {
                c = c >> 1;
            }
        }
        crc32_fast[0][n] = c ^ 0xff000000;
    }
}

int main ()
{
    __slz_make_crc_table();
    printf ("%d", crc32_fast[0][0]);
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
