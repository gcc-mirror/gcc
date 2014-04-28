/* { dg-options "-O2" } */
/* { dg-do run } */

extern void abort (void);

typedef unsigned int __u32;

__u32
__rev16_32_alt (__u32 x)
{
  return (((__u32)(x) & (__u32)0xff00ff00UL) >> 8)
         | (((__u32)(x) & (__u32)0x00ff00ffUL) << 8);
}

__u32
__rev16_32 (__u32 x)
{
  return (((__u32)(x) & (__u32)0x00ff00ffUL) << 8)
         | (((__u32)(x) & (__u32)0xff00ff00UL) >> 8);
}

typedef unsigned long long __u64;

__u64
__rev16_64_alt (__u64 x)
{
  return (((__u64)(x) & (__u64)0xff00ff00ff00ff00UL) >> 8)
         | (((__u64)(x) & (__u64)0x00ff00ff00ff00ffUL) << 8);
}

__u64
__rev16_64 (__u64 x)
{
  return (((__u64)(x) & (__u64)0x00ff00ff00ff00ffUL) << 8)
         | (((__u64)(x) & (__u64)0xff00ff00ff00ff00UL) >> 8);
}

int
main (void)
{
  volatile __u32 in32 = 0x12345678;
  volatile __u32 expected32 = 0x34127856;
  volatile __u64 in64 = 0x1234567890abcdefUL;
  volatile __u64 expected64 = 0x34127856ab90efcdUL;

  if (__rev16_32 (in32) != expected32)
    abort ();

  if (__rev16_32_alt (in32) != expected32)
    abort ();

  if (__rev16_64 (in64) != expected64)
    abort ();

  if (__rev16_64_alt (in64) != expected64)
    abort ();

  return 0;
}
