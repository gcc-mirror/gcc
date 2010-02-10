/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int ffs (int) __asm ("__GI_ffs") __attribute__ ((nothrow, const));

int
ffsll (long long int i)
{
  unsigned long long int x = i & -i;
  
  if (x <= 0xffffffff)
    return ffs (i);
  else
    return 32 + ffs (i >> 32);
}

/* { dg-final { scan-assembler-not "\nffs\n|\nffs\[^a-zA-Z0-9_\]|\[^a-zA-Z0-9_\]ffs\n" } } */
