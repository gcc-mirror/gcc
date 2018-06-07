/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds -fdump-tree-phiopt1" } */

void
h (const void *p, unsigned n)
{
  unsigned char a[8];
  if (n > sizeof a)
    return;

  for (; n > 0; n -= *a)
    {
      if (n > 255)
	*a = 255;
      else
	*a = n;

      __builtin_memcpy (a, p, *a);   /* { dg-bogus "bounds" } */
    }
}

/* { dg-final { scan-tree-dump "MIN_EXPR" "phiopt1" } } */
