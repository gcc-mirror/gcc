/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fno-tree-vectorize -fno-tree-loop-distribute-patterns -fdump-tree-ivopts" } */

/* The counter can be eliminated in favour of P, since the offsets I and N
   are known to be non-negative and small enough not to overflow.  Note that
   this is not the case for offsets of the width of a pointer, see
   gcc.dg/torture/ivopts-lt-1.c.  */

void
f1 (char *p, unsigned int i, unsigned int n)
{
  p += i;
  do
    {
      *p = '\0';
      p += 1;
      i++;
    }
  while (i < n);
}

/* { dg-final { scan-tree-dump-times "PHI" 1 "ivopts" { target { ! powerpc*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "PHI" 2 "ivopts" { target { powerpc*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "PHI <p_" 1 "ivopts" { target { ! powerpc*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "p_\[0-9\]* <" 1 "ivopts" { target { ! powerpc*-*-* } } } } */
