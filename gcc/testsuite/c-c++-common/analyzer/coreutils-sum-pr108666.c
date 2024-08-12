/* Reduced from coreutils's sum.c: bsd_sum_stream */

typedef __SIZE_TYPE__ size_t;
typedef unsigned char __uint8_t;
typedef unsigned long int __uintmax_t;
typedef struct _IO_FILE FILE;
extern size_t
fread_unlocked(void* __restrict __ptr,
               size_t __size,
               size_t __n,
               FILE* __restrict __stream);
extern int
feof_unlocked(FILE* __stream) __attribute__((__nothrow__, __leaf__));
extern int
ferror_unlocked(FILE* __stream) __attribute__((__nothrow__, __leaf__));
extern void*
memcpy(void* __restrict __dest, const void* __restrict __src, size_t __n)
  __attribute__((__nothrow__, __leaf__)) __attribute__((__nonnull__(1, 2)));
extern void
rpl_free(void*);
extern int*
__errno_location(void) __attribute__((__nothrow__, __leaf__))
__attribute__((__const__));
extern void*
malloc(size_t __size) __attribute__((__nothrow__, __leaf__))
__attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
typedef __uint8_t uint8_t;
typedef __uintmax_t uintmax_t;

int
bsd_sum_stream(FILE* stream, void* resstream, uintmax_t* length)
{
  int ret = -1;
  size_t sum, n;
  int checksum = 0;
  uintmax_t total_bytes = 0;
  static const size_t buffer_length = 32768;
  uint8_t* buffer = (uint8_t *) malloc(buffer_length); /* { dg-warning "argument 1 value '32768' exceeds maximum object size 32767" "" { target { c++ && { ! size20plus } } } } */

  if (!buffer)
    return -1;

  while (1) {
    sum = 0;

    while (1) {
      n = fread_unlocked(buffer + sum, 1, buffer_length - sum, stream);
      sum += n;

      if (buffer_length == sum)
        break;

      if (n == 0) {
        if (ferror_unlocked(stream))
          goto cleanup_buffer;
        goto final_process;
      }

      if (feof_unlocked(stream))
        goto final_process;
    }

    for (size_t i = 0; i < sum; i++) {
      checksum = (checksum >> 1) + ((checksum & 1) << 15);
      checksum += buffer[i]; /* { dg-bogus "use of uninitialized value" "PR analyzer/108666" } */
      checksum &= 0xffff;
    }
    if (total_bytes + sum < total_bytes) {

      (*__errno_location()) = 75;
      goto cleanup_buffer;
    }
    total_bytes += sum;
  }

final_process:;

  for (size_t i = 0; i < sum; i++) {
    checksum = (checksum >> 1) + ((checksum & 1) << 15);
    checksum += buffer[i];  /* { dg-bogus "use of uninitialized value" "PR analyzer/108666" } */
    checksum &= 0xffff;
  }
  if (total_bytes + sum < total_bytes) {

    (*__errno_location()) = 75;
    goto cleanup_buffer;
  }
  total_bytes += sum;

  memcpy(resstream, &checksum, sizeof checksum);
  *length = total_bytes;
  ret = 0;
cleanup_buffer:

  rpl_free(buffer);
  return ret;
}

