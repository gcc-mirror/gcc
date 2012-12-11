/* PR target/54121 */
/* Reported by Jan Engelhardt <jengelh@inai.de> */

/* { dg-do compile { target fpic } } */
/* { dg-options "-std=gnu99 -O -fPIC -fprofile-generate" } */

typedef __SIZE_TYPE__ size_t;
typedef unsigned char uint8_t;

extern void *memcpy (void *__restrict __dest,
       __const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

typedef enum {
 LZMA_OK = 0,
 LZMA_STREAM_END = 1,
 LZMA_NO_CHECK = 2,
 LZMA_UNSUPPORTED_CHECK = 3,
 LZMA_GET_CHECK = 4,
 LZMA_MEM_ERROR = 5,
 LZMA_MEMLIMIT_ERROR = 6,
 LZMA_FORMAT_ERROR = 7,
 LZMA_OPTIONS_ERROR = 8,
 LZMA_DATA_ERROR = 9,
 LZMA_BUF_ERROR = 10,
 LZMA_PROG_ERROR = 11,
} lzma_ret;

typedef enum {
 LZMA_RUN = 0,
 LZMA_SYNC_FLUSH = 1,
 LZMA_FULL_FLUSH = 2,
 LZMA_FINISH = 3
} lzma_action;

typedef struct {
 void *( *alloc)(void *opaque, size_t nmemb, size_t size);
 void ( *free)(void *opaque, void *ptr);
 void *opaque;
} lzma_allocator;

typedef struct lzma_coder_s lzma_coder;

typedef struct lzma_next_coder_s lzma_next_coder;

typedef struct lzma_filter_info_s lzma_filter_info;

typedef lzma_ret (*lzma_init_function)(
  lzma_next_coder *next, lzma_allocator *allocator,
  const lzma_filter_info *filters);

typedef lzma_ret (*lzma_code_function)(
  lzma_coder *coder, lzma_allocator *allocator,
  const uint8_t *restrict in, size_t *restrict in_pos,
  size_t in_size, uint8_t *restrict out,
  size_t *restrict out_pos, size_t out_size,
  lzma_action action);

typedef void (*lzma_end_function)(
  lzma_coder *coder, lzma_allocator *allocator);

typedef struct {
 uint8_t *buf;
 size_t pos;
 size_t size;
} lzma_dict;

typedef struct {
 lzma_coder *coder;
 lzma_ret (*code)(lzma_coder *restrict coder,
   lzma_dict *restrict dict, const uint8_t *restrict in,
   size_t *restrict in_pos, size_t in_size);
} lzma_lz_decoder;

struct lzma_coder_s {
 lzma_dict dict;
 lzma_lz_decoder lz;
};

lzma_ret
decode_buffer(lzma_coder *coder,
  const uint8_t *restrict in, size_t *restrict in_pos,
  size_t in_size, uint8_t *restrict out, size_t *restrict out_pos)
{
 while (1) {
  const size_t dict_start = coder->dict.pos;
  const lzma_ret ret
    = coder->lz.code( coder->lz.coder, &coder->dict, in, in_pos, in_size);
  const size_t copy_size = coder->dict.pos - dict_start;
  memcpy(out + *out_pos, coder->dict.buf + dict_start, copy_size);
  if (ret != LZMA_OK || coder->dict.pos < coder->dict.size)
   return ret;
 }
}
