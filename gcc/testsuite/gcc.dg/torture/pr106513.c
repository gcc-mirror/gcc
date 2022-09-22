/* { dg-do run } */

typedef __INT64_TYPE__ int64_t;

__attribute__((noinline)) int64_t
swap64 (int64_t n)
{
  return (((n & (((int64_t) 0xff) )) << 56) |
          ((n & (((int64_t) 0xff) << 8)) << 40) |
          ((n & (((int64_t) 0xff) << 16)) << 24) |
          ((n & (((int64_t) 0xff) << 24)) << 8) |
          ((n & (((int64_t) 0xff) << 32)) >> 8) |
          ((n & (((int64_t) 0xff) << 40)) >> 24) |
          ((n & (((int64_t) 0xff) << 48)) >> 40) |
          ((n & ((int64_t)(0xffull << 56))) >> 56));
}

int main (void)
{
  volatile int64_t n = 0x8000000000000000ll;

  if (swap64(n) != 0xffffffffffffff80ll)
    __builtin_abort ();

  return 0;
}
