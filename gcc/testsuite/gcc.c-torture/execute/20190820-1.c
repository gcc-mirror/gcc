/* PR rtl-optimization/91347 */
/* Reported by John David Anglin <danglin@gcc.gnu.org> */
/* { dg-require-effective-target int32plus } */

typedef unsigned short __u16;
typedef __signed__ int __s32;
typedef unsigned int __u32;
typedef __signed__ long long __s64;
typedef unsigned long long __u64;
typedef __u16 u16;
typedef __s32 s32;
typedef __u32 u32;
typedef __u64 u64;
typedef _Bool bool;
typedef s32 int32_t;
typedef u32 uint32_t;
typedef u64 uint64_t;

char hex_asc_upper[16];
u16 decpair[100];

static __attribute__ ((noipa)) void
put_dec_full4 (char *buf, unsigned r)
{
 unsigned q;
 q = (r * 0x147b) >> 19;
 *((u16 *)buf) = decpair[r - 100*q];
 buf += 2;
 *((u16 *)buf) = decpair[q];
}

static __attribute__ ((noipa)) unsigned
put_dec_helper4 (char *buf, unsigned x)
{
  uint32_t q = (x * (uint64_t)0x346DC5D7) >> 43;
  put_dec_full4(buf, x - q * 10000);
  return q;
}

static __attribute__ ((noipa)) char *
put_dec (char *buf, unsigned long long n)
{
 uint32_t d3, d2, d1, q, h;
 d1 = ((uint32_t)n >> 16);
 h = (n >> 32);
 d2 = (h ) & 0xffff;
 d3 = (h >> 16);
 q = 656 * d3 + 7296 * d2 + 5536 * d1 + ((uint32_t)n & 0xffff);
 q = put_dec_helper4(buf, q);
 q += 7671 * d3 + 9496 * d2 + 6 * d1;
 q = put_dec_helper4(buf+4, q);
 q += 4749 * d3 + 42 * d2;
 q = put_dec_helper4(buf+8, q);
 return buf;
}

struct printf_spec {
 unsigned int type:8;
 signed int field_width:24;
 unsigned int flags:8;
 unsigned int base:8;
 signed int precision:16;
} __attribute__((__packed__));

static __attribute__ ((noipa)) char *
number (char *buf, char *end, unsigned long long num, struct printf_spec spec)
{

 char tmp[3 * sizeof(num)] __attribute__((__aligned__(2)));
 char sign;
 char locase;
 int need_pfx = ((spec.flags & 64) && spec.base != 10);
 int i;
 bool is_zero = num == 0LL;
 int field_width = spec.field_width;
 int precision = spec.precision;

 i = 0;
 if (num < spec.base)
  tmp[i++] = hex_asc_upper[num] | locase;
 else if (spec.base != 10) {
  int mask = spec.base - 1;
  int shift = 3;
  if (spec.base == 16)
   shift = 4;
  else
    __builtin_abort ();
  do {
   tmp[i++] = (hex_asc_upper[((unsigned char)num) & mask] | locase);
   num >>= shift;
  } while (num);
 } else {
  i = put_dec(tmp, num) - tmp;
 }
 return buf;
}

static __attribute__ ((noipa)) char *
pointer_string (char *buf, char *end, const void *ptr, struct printf_spec spec)
{
 spec.base = 16;
 spec.flags = 0;
 return number(buf, end, 100, spec);
}

int
main (void)
{
  struct printf_spec spec;
  char *s = pointer_string (0, 0, 0, spec);
  return 0;
}
