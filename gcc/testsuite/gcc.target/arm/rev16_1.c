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

int
main (void)
{
  volatile __u32 in32 = 0x12345678;
  volatile __u32 expected32 = 0x34127856;

  if (__rev16_32 (in32) != expected32)
    abort ();

  if (__rev16_32_alt (in32) != expected32)
    abort ();

  return 0;
}
