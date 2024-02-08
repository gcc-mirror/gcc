/* { dg-options "-O2" } */
/* { dg-do compile } */

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

/* { dg-final { scan-assembler-times {rev16\tr[0-9]+, r[0-9]+} 2 { xfail arm_thumb1_ok } } } */