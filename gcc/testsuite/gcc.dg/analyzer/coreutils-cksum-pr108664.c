/* Reduced from coreutils's cksum.c: cksum_slice8 */

typedef long unsigned int size_t;
typedef unsigned int __uint32_t;
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
static __inline __uint32_t
__bswap_32(__uint32_t __bsx)
{

  return __builtin_bswap32(__bsx);
}
typedef __uint32_t uint32_t;
typedef unsigned long int uint_fast32_t;
typedef __uintmax_t uintmax_t;
extern int*
__errno_location(void) __attribute__((__nothrow__, __leaf__))
__attribute__((__const__));
extern uint_fast32_t const crctab[8][256];

static _Bool
cksum_slice8(FILE* fp, uint_fast32_t* crc_out, uintmax_t* length_out)
{
  uint32_t buf[(1 << 16) / sizeof(uint32_t)];
  uint_fast32_t crc = 0;
  uintmax_t length = 0;
  size_t bytes_read;

  if (!fp || !crc_out || !length_out)
    return 0;

  while ((bytes_read = fread_unlocked(buf, 1, (1 << 16), fp)) > 0) {
    uint32_t* datap;

    if (length + bytes_read < length) {

      (*__errno_location()) = 75;
      return 0;
    }
    length += bytes_read;

    if (bytes_read == 0) {
      if (ferror_unlocked(fp))
        return 0;
    }

    datap = (uint32_t*)buf;
    while (bytes_read >= 8) {
      uint32_t first = *datap++, second = *datap++; /* { dg-bogus "use of uninitialized value" "PR analyzer/108664" } */
      crc ^= __bswap_32(first);
      second = __bswap_32(second);
      crc =
        (crctab[7][(crc >> 24) & 0xFF] ^ crctab[6][(crc >> 16) & 0xFF] ^
         crctab[5][(crc >> 8) & 0xFF] ^ crctab[4][(crc)&0xFF] ^
         crctab[3][(second >> 24) & 0xFF] ^ crctab[2][(second >> 16) & 0xFF] ^
         crctab[1][(second >> 8) & 0xFF] ^ crctab[0][(second)&0xFF]);
      bytes_read -= 8;
    }

    unsigned char* cp = (unsigned char*)datap;
    while (bytes_read--)
      crc = (crc << 8) ^ crctab[0][((crc >> 24) ^ *cp++) & 0xFF];
    if (feof_unlocked(fp))
      break;
  }

  *crc_out = crc;
  *length_out = length;

  return 1;
}
