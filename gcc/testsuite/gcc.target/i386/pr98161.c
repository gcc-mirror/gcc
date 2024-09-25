/* { dg-do run } */
/* { dg-options "-O2 -msse4" } */
/* { dg-require-effective-target sse4 } */

typedef unsigned short u16;
typedef unsigned int   u32;
typedef unsigned char  u8;

u32
__attribute__((__force_align_arg_pointer__))
unreach(const u16 * pu16, u16 *dst, u32 dstlen, const u8 *src, u32 srclen)
{
  for (u32 i = dstlen; srclen && i; i--, srclen--, src++, dst++)
    {
      u16 off = pu16[*src];
      if (off)
	{
	  src++; srclen--;
	  *dst = pu16[off + *src];
	}
    }
  return 56;
}

u32
__attribute__((__force_align_arg_pointer__))
__attribute__((noipa))
bug(const u16 * pu16, u16 *dst, u32 dstlen, const u8 *src, u32 srclen)
{
  if (pu16)
    /* Branch should not execute, but stack realignment
     * reads wrong 'pu16' value from stack. */
    return unreach(pu16, dst, dstlen, src, srclen);

  return (srclen < dstlen) ? srclen : dstlen;
}

int
main()
{
  if (__builtin_cpu_supports ("sse4.1"))
    {
      /* Should return 12 */
      if (bug(0, 0, 12, 0, 34) != 12)
	__builtin_abort ();
    }
  return 0;
}
