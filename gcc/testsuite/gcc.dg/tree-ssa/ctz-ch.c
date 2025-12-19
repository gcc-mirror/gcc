/* { dg-do compile } */
/* { dg-require-effective-target ctz } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned long BITMAP_WORD;

bool
bmp_iter_set (BITMAP_WORD bits, unsigned *bit_no)
{
  /* If our current word is nonzero, it contains the bit we want.  */
  if (bits)
    {
      while (!(bits & 1))
	{
	  bits >>= 1;
	  *bit_no += 1;
	}
      return true;
    }

  return false;
}

/* { dg-final { scan-tree-dump-times "__builtin_ctz|\\.CTZ" 1 "optimized" } } */
