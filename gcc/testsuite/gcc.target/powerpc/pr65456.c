/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc64le-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3" } */

/* Verify that swap optimization properly removes swaps for unaligned
   vector stores.  See PR65456.  */

typedef unsigned char UChar;
typedef unsigned short UShort;
typedef unsigned int UWord;

typedef unsigned long SizeT;
typedef unsigned long Addr;

void *memmove(void *dst, const void *src, SizeT len)
{
  const Addr WS = sizeof(UWord);/* 8 or 4 */
  const Addr WM = WS - 1;/* 7 or 3 */

  /* Copying backwards. */
  SizeT n = len;
  Addr d = (Addr) dst;
  Addr s = (Addr) src;

  if (((s ^ d) & WM) == 0) {
    /* s and d have same UWord alignment. */
    /* Pull up to a UWord boundary. */
    while ((s & WM) != 0 && n >= 1) {
      *(UChar *) d = *(UChar *) s;
      s += 1;
      d += 1;
      n -= 1;
    }
    /* Copy UWords. */
    while (n >= WS) {
      *(UWord *) d = *(UWord *) s;
      s += WS;
      d += WS;
      n -= WS;
    }
    if (n == 0)
      return dst;
  }
  if (((s | d) & 1) == 0) {
    /* Both are 16-aligned; copy what we can thusly. */
    while (n >= 2) {
      *(UShort *) d = *(UShort *) s;
      s += 2;
      d += 2;
      n -= 2;
    }
  }
  /* Copy leftovers, or everything if misaligned. */
  while (n >= 1) {
    *(UChar *) d = *(UChar *) s;
    s += 1;
    d += 1;
    n -= 1;
  }

  return dst;
}

/* { dg-final { scan-assembler-not "xxpermdi" } } */
/* { dg-final { scan-assembler-not "xxswapd" } } */
