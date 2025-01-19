/* { dg-options "-O2 -fdump-rtl-ext_dce" } */
typedef unsigned char __uint8_t;
typedef unsigned int __uint32_t;
typedef __uint8_t uint8_t;
typedef __uint32_t uint32_t;
static inline void
unaligned_write32le(uint8_t *buf, uint32_t num)
{
 buf[0] = num;
 buf[1] = num >> 8;
 buf[2] = num >> 16;
 buf[3] = num >> 24;
 return;
}
typedef struct {
 uint32_t dict_size;
} lzma_options_lzma;
typedef void lzma_coder;
typedef struct lzma_next_coder_s lzma_next_coder;
struct lzma_next_coder_s {
 lzma_coder *coder;
};
struct lzma_coder_s {
 uint8_t header[(1 + 4 + 8)];
};

void
alone_encoder_init(lzma_next_coder *next, const lzma_options_lzma *options)
{
 uint32_t d = options->dict_size - 1;
 d |= d >> 2;
#if 0
 d |= d >> 3;
 d |= d >> 4;
 d |= d >> 8;
 d |= d >> 16;
#endif
 if (d != (4294967295U))
  ++d;
 unaligned_write32le(((struct lzma_coder_s*)next->coder)->header + 1, d);
}

/* { dg-final { scan-rtl-dump "Successfully transformed to:" "ext_dce" } } */

