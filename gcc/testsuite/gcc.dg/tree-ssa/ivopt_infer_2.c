/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-ivopts-details" } */

#ifndef TYPE
#define TYPE char*
#endif

extern char a[];

/* Can not infer loop iteration from array -- exit test can not be
   replaced by the array address.  */
void foo (unsigned int i_width, TYPE dst)
{
  unsigned long long i = 0;
  unsigned long long j = 0;
  for ( ; j < i_width; )
    {
      *dst = a[i];
      dst++;
      i += 2;
      j += 1;
    }
}

/* { dg-final { scan-tree-dump-times "\[^:\]*if \\(.*j_\[0-9\]+.*\\)" 1 "ivopts"} } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
