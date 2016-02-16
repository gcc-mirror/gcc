/* { dg-do run } */
/* { dg-options "-fno-strict-aliasing" } */

#include <stdint.h>
#include <stdio.h>

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define av_le2ne32(x) (x)
#else
#define av_le2ne32(x) av_bswap32(x)
#endif

static __attribute__((always_inline)) inline __attribute__((const)) uint32_t av_bswap32(uint32_t x)
{
    return ((((x) << 8 & 0xff00) | ((x) >> 8 & 0x00ff)) << 16 | ((((x) >> 16) << 8 & 0xff00) | (((x) >> 16) >> 8 & 0x00ff)));
}

typedef uint32_t AVCRC;

typedef enum {
    AV_CRC_8_ATM,
    AV_CRC_16_ANSI,
    AV_CRC_16_CCITT,
    AV_CRC_32_IEEE,
    AV_CRC_32_IEEE_LE,
    AV_CRC_16_ANSI_LE,
    AV_CRC_24_IEEE = 12,
    AV_CRC_MAX,
} AVCRCId;

int av_crc_init(AVCRC *ctx, int le, int bits, uint32_t poly, int ctx_size);






uint32_t av_crc(const AVCRC *ctx, uint32_t crc,
                const uint8_t *buffer, size_t length) __attribute__((pure));
static struct {
    uint8_t le;
    uint8_t bits;
    uint32_t poly;
} av_crc_table_params[AV_CRC_MAX] = {
    [AV_CRC_8_ATM] = { 0, 8, 0x07 },
    [AV_CRC_16_ANSI] = { 0, 16, 0x8005 },
    [AV_CRC_16_CCITT] = { 0, 16, 0x1021 },
    [AV_CRC_24_IEEE] = { 0, 24, 0x864CFB },
    [AV_CRC_32_IEEE] = { 0, 32, 0x04C11DB7 },
    [AV_CRC_32_IEEE_LE] = { 1, 32, 0xEDB88320 },
    [AV_CRC_16_ANSI_LE] = { 1, 16, 0xA001 },
};
static AVCRC av_crc_table[AV_CRC_MAX][1024];


int av_crc_init(AVCRC *ctx, int le, int bits, uint32_t poly, int ctx_size)
{
    unsigned i, j;
    uint32_t c;

    if (bits < 8 || bits > 32 || poly >= (1LL << bits))
        return -1;
    if (ctx_size != sizeof(AVCRC) * 257 && ctx_size != sizeof(AVCRC) * 1024)
        return -1;

    for (i = 0; i < 256; i++) {
        if (le) {
            for (c = i, j = 0; j < 8; j++)
                c = (c >> 1) ^ (poly & (-(c & 1)));
            ctx[i] = c;
        } else {
            for (c = i << 24, j = 0; j < 8; j++)
                c = (c << 1) ^ ((poly << (32 - bits)) & (((int32_t) c) >> 31));
            ctx[i] = av_bswap32(c);
        }
    }
    ctx[256] = 1;

    if (ctx_size >= sizeof(AVCRC) * 1024)
        for (i = 0; i < 256; i++)
            for (j = 0; j < 3; j++)
                ctx[256 *(j + 1) + i] =
                    (ctx[256 * j + i] >> 8) ^ ctx[ctx[256 * j + i] & 0xFF];


    return 0;
}

const AVCRC *av_crc_get_table(AVCRCId crc_id)
{
    if (!av_crc_table[crc_id][(sizeof(av_crc_table[crc_id]) / sizeof((av_crc_table[crc_id])[0])) - 1])
        if (av_crc_init(av_crc_table[crc_id],
                        av_crc_table_params[crc_id].le,
                        av_crc_table_params[crc_id].bits,
                        av_crc_table_params[crc_id].poly,
                        sizeof(av_crc_table[crc_id])) < 0)
            return ((void *)0);

    return av_crc_table[crc_id];
}

uint32_t av_crc(const AVCRC *ctx, uint32_t crc,
                const uint8_t *buffer, size_t length)
{
    const uint8_t *end = buffer + length;


    if (!ctx[256]) {
        while (((intptr_t) buffer & 3) && buffer < end)
            crc = ctx[((uint8_t) crc) ^ *buffer++] ^ (crc >> 8);

        while (buffer < end - 3) {
            crc ^= av_le2ne32(*(const uint32_t *) buffer); buffer += 4;
            crc = ctx[3 * 256 + ( crc & 0xFF)] ^
                  ctx[2 * 256 + ((crc >> 8 ) & 0xFF)] ^
                  ctx[1 * 256 + ((crc >> 16) & 0xFF)] ^
                  ctx[0 * 256 + ((crc >> 24) )];
        }
    }

    while (buffer < end)
        crc = ctx[((uint8_t) crc) ^ *buffer++] ^ (crc >> 8);

    return crc;
}


int main(void)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ || __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint8_t buf[1999];
    int i;
    unsigned
      p[6][3] = { { AV_CRC_32_IEEE_LE, 0xEDB88320, 0x3D5CDD04 },
		  { AV_CRC_32_IEEE , 0x04C11DB7, 0xE0BAF5C0 },
		  { AV_CRC_24_IEEE , 0x864CFB , 0x326039 },
		  { AV_CRC_16_ANSI_LE, 0xA001 , 0xBFD8 },
		  { AV_CRC_16_ANSI , 0x8005 , 0xBB1F },
		  { AV_CRC_8_ATM , 0x07 , 0xE3 }
    };
    const AVCRC *ctx;

    for (i = 0; i < sizeof(buf); i++)
        buf[i] = i + i * i;

    for (i = 0; i < 6; i++) {
        int id = p[i][0];
	uint32_t result;
        ctx = av_crc_get_table (id);
	result = av_crc(ctx, 0, buf, sizeof(buf));
	if (result != p[i][2])
	  __builtin_abort ();
    }
#endif
    return 0;
}
