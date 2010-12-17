/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-ivopts-details" } */

#ifndef TYPE
#define TYPE char*
#endif

extern int a[];

/* Can not infer loop iteration from array -- exit test can not be replaced.  */
void foo (int i_width, TYPE dst, TYPE src1, TYPE src2)
{
      TYPE dstn= dst + i_width;
      TYPE dst0 = dst;
      unsigned long long i = 0;
       for( ; dst <= dstn; )
       {
           dst0[i] = ( src1[i] + src2[i] + 1 +a[i]) >> 1;
           dst++;
	   i += 16;
       }
}

/* { dg-final { scan-tree-dump-times "Replacing" 0 "ivopts"} } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
