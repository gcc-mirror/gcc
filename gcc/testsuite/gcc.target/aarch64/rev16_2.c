/* { dg-options "-O2" } */
/* { dg-do compile } */

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

/* { dg-final { scan-assembler-times "rev16\\tx\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "rev16\\tw\[0-9\]+" 2 } } */
