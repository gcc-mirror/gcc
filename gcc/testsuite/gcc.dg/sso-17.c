/* { dg-do run } */
/* { dg-options "-O2" } */

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define REV_ENDIANNESS __attribute__((scalar_storage_order("big-endian")))
#else
#define REV_ENDIANNESS __attribute__((scalar_storage_order("little-endian")))
#endif

typedef unsigned long long u64;

union DST {
  u64 val;

  struct {
    u64 x : 1;
    u64 y : 1;
    u64 r: 62;
  } REV_ENDIANNESS;
} REV_ENDIANNESS;


struct SRC {
  u64 a;
} REV_ENDIANNESS;

[[gnu::noipa]]
void foo () {__builtin_abort();}

[[gnu::noinline]]
int bar(struct SRC *src)
{
  union DST dst;
  
  dst.val = src->a;

  if (dst.y) {
    foo();
  }
  return 0;
}

int main(void)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  struct SRC t = {-1ull & (~(0x01ull<<62))};
#else
  struct SRC t = {-1ull & (~(0x01ull<<1))};
#endif
  bar(&t);
  return 0;
}
